%%% vim:foldmethod=marker
-module(test).  % {{{1

-behaviour(gen_server).

%% API
-export([
         start_link/0, start_link/1,
         go/0,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {count, status_bar}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API   {{{1

start_link() ->
    start_link(20).

start_link(Count) ->
    lm:start_link(?MODULE),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Count, []).

stop() ->
    gen_server:call(?SERVER, stop).

go() ->
    reset_state(count()),
    gen_server:call(?SERVER, begin_test).

%%% gen_server callbacks   {{{1

init(Count) ->
    reset_state(Count).

handle_call(begin_test, _From, State) ->
    erlang:send_after(10, self(), {try_request, 1}),
    {reply, ok, State};
handle_call(count, _From, #state{count=Count}=State) ->
    {reply, Count, State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({request, Id, no_such_category}, #state{status_bar=StatusBar}=State) ->
    erlang:send_after(5000, self(), {retry_request, Id}),
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "A")}};
handle_cast({request, Id, already_granted}, #state{status_bar=StatusBar}=State) ->
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "B")}};
handle_cast({request, Id, granted}, #state{status_bar=StatusBar}=State) ->
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "+")}};
handle_cast({request, Id, denied}, #state{status_bar=StatusBar}=State) ->
    erlang:send_after(750, self(), {retry_request, Id}),
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "x")}};
handle_cast({release, Id, no_such_category}, #state{status_bar=StatusBar}=State) ->
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "C")}};
handle_cast({release, Id, released}, #state{status_bar=StatusBar}=State) ->
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "-")}};
handle_cast({release, Id, guid_is_not_using_license}, #state{status_bar=StatusBar}=State) ->
    {noreply, State#state{status_bar=update_bar(StatusBar, Id, "D")}};
handle_cast({update_license_info, NewCategories}, State) ->
    io:format("Updated licenses: ~p~n", [NewCategories]),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({retry_request, Id}, #state{status_bar=StatusBar}=State) ->
    lm:request(voice, Id),
    {noreply, State};
    % {noreply, State#state{status_bar=update_bar(StatusBar, Id, "?")}};
handle_info({try_request, Count}, #state{count=Count, status_bar=StatusBar}=State) ->
    lm:request(voice, Count),
    {noreply, State};
    % {noreply, State#state{status_bar=update_bar(StatusBar, Count, "?")}};
handle_info({try_request, Id}, #state{status_bar=StatusBar}=State) ->
    lm:request(voice, Id),
    erlang:send_after(100 + random:uniform(3000), self(), {try_request, Id+1}),
    {noreply, State}.
    % {noreply, State#state{status_bar=update_bar(StatusBar, Id, "?")}};

terminate(_Reason, _State) ->
    lm:stop(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions   {{{1

count() ->
    gen_server:call(?SERVER, count).

reset_state(Count) ->
    {ok, #state{count=Count, status_bar=string:left(".", Count, $.)}}.

update_bar(StatusBar, Id, Value) ->
    NewStatusBar = string:concat(string:concat(string:sub_string(StatusBar,1,Id-1), Value), string:sub_string(StatusBar,Id+1)),
    io:format("~p~n",[NewStatusBar]),
    NewStatusBar.

%%% Unit Tests   {{{1
-ifdef(EUNIT).
-endif.
