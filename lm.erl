%%% vim:foldmethod=marker
-module(lm).  % {{{1
-behaviour(gen_server).

%% API
-export([
         start_link/1,
         stop/0,
         request/2,
         release/2,
         state/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(category, {name, max=undefined, in_use=[]}).
-record(state, {parent, categories=[], update_function}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API   {{{1

start_link(Parent) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Parent, []).

stop() ->
    gen_server:call(?SERVER, stop).

request(Category, Guid) ->
    gen_server:call(?SERVER, {request, Category, Guid}).

release(Category, Guid) ->
    gen_server:call(?SERVER, {release, Category, Guid}).

state() ->
    gen_server:call(?SERVER, state).

%%% gen_server callbacks   {{{1

init(Parent) ->
    license_poller:start_link(2000),
    {ok, #state{parent=Parent}}.

handle_call(state, _From, State) ->
    {reply, State, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call({request, Category, Guid}, _From, State) ->
    request_license(Category, Guid, State);
handle_call({release, Category, Guid}, _From, State) ->
    release_license(Category, Guid, State).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({expire, Category, Guid}, State) ->
    {reply, _, NewState} = release_license(Category, Guid, State),
    {noreply, NewState};
handle_info({update_license_info, LicenseFiles}, State) ->
    NewState = read_license_files(LicenseFiles, State),
    {noreply, NewState}.

terminate(_Reason, _State) ->
    license_poller:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions   {{{1

request_license(false, Guid, #state{parent=Parent}=State) ->   %{{{2
    gen_server:cast(Parent, {request, Guid, no_such_category}),
    {reply, no_such_category, State};
request_license(#category{in_use=InUse}=Category, Guid, State) ->
    request_license(Category, Guid, lists:member(Guid, InUse), State);
request_license(Category, Guid, #state{categories=Categories}=State) ->
    request_license(lists:keyfind(Category, #category.name, Categories), Guid, State).

request_license(#category{}=_Category, Guid, true, #state{parent=Parent}=State) ->
    gen_server:cast(Parent, {request, Guid, already_granted}),
    {reply, already_granted, State};
request_license(#category{name=Name, max=Max, in_use=InUse}=Category, Guid, false, #state{categories=Categories, parent=Parent}=State) when length(InUse) < Max ->
    erlang:send_after(15000, self(), {expire, Name, Guid}),
    gen_server:cast(Parent, {request, Guid, granted}),
    {reply, ok, State#state{categories=lists:keyreplace(Name, #category.name, Categories, Category#category{in_use=[Guid|InUse]})}};
request_license(#category{}=_Category, Guid, false, #state{parent=Parent}=State) ->
    gen_server:cast(Parent, {request, Guid, denied}),
    {reply, not_available, State}.

release_license(false, Guid, #state{parent=Parent}=State) ->   %{{{2
    gen_server:cast(Parent, {release, Guid, no_such_category}),
    {reply, no_such_category, State};
release_license(#category{in_use=InUse}=Category, Guid, State) ->
    release_license(Category, Guid, lists:member(Guid, InUse), State);
release_license(Category, Guid, #state{categories=Categories}=State) ->
    release_license(lists:keyfind(Category, #category.name, Categories), Guid, State).

release_license(#category{name=Name, in_use=InUse}=Category, Guid, true, #state{categories=Categories, parent=Parent}=State) ->
    gen_server:cast(Parent, {release, Guid, released}),
    {reply, ok, State#state{categories=lists:keyreplace(Name, #category.name, Categories, Category#category{in_use=lists:delete(Guid, InUse)})}};
release_license(_Category, Guid, false, #state{parent=Parent}=State) ->
    gen_server:cast(Parent, {release, Guid, guid_is_not_using_license}),
    {reply, guid_is_not_using_license, State}.

read_license_files(LicenseFiles, State) ->   %{{{2
    NewLicenseSpecs = lists:flatten(lists:map(
        fun({File,_ModDate}) ->
                {ok,Spec} = file:consult(File),
                proplists:get_value(licenses, Spec)
        end, LicenseFiles)),
    NewLicenseTotals = lists:map(
        fun(UniqueKey) ->
                {UniqueKey, lists:foldl(
                             fun({Category, Max}, Acc) when Category == UniqueKey -> Acc+Max;
                                (_, Acc) -> Acc
                             end, 0, NewLicenseSpecs)}
        end, proplists:get_keys(NewLicenseSpecs)),
    update_license_info(NewLicenseTotals, State).

update_license_info(NewLicenseTotals, #state{categories=CurrentCategories, parent=Parent}=State) ->
    UpdatedCategories = lists:map(
                          fun({Name, NewMax}) ->
                                  update_category(lists:keyfind(Name, #category.name, CurrentCategories), Name, NewMax)
                          end,
                          NewLicenseTotals),
    DisabledCategories = lists:filtermap(
                           fun(#category{name=Name}=Category) ->
                                   disable_category(lists:keyfind(Name, 1, NewLicenseTotals), Category)
                           end, CurrentCategories),
    NewState = State#state{categories=UpdatedCategories ++ DisabledCategories},
    gen_server:cast(Parent, {update_license_info, NewState#state.categories}),
    NewState.

update_category(false, Name, NewMax) ->
    #category{name=Name, max=NewMax};
update_category(Category, _Name, NewMax) ->
    Category#category{max=NewMax}.

disable_category(false, Category) ->
    {true, Category#category{max=0}};
disable_category(_NewTotal, _Category) ->
    false.


%%% Unit Tests   {{{1
-ifdef(EUNIT).
-endif.
