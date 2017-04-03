%%% vim:foldmethod=marker
-module(license_poller).  % {{{1

-behaviour(gen_server).

%% API
-export([
         start_link/1,
         stop/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).
-define(LICENSE_FILE_LOCATION, "/Users/prunninger/projects/spikes/license_manager").

-record(state, {parent, polling_interval=10000, license_files=[]}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%% API   {{{1

start_link(PollingInterval) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {self(), PollingInterval}, []).

stop() ->
    gen_server:call(?SERVER, stop).

%%% gen_server callbacks   {{{1

init({Pid, PollingInterval}) ->
    erlang:send_after(100, self(), poll),
    {ok, #state{parent=Pid, polling_interval=PollingInterval, license_files=[]}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll, State) ->
    NewFiles = get_files(State#state.parent, State#state.license_files),
    erlang:send_after(State#state.polling_interval, self(), poll),
    {noreply, State#state{license_files=NewFiles}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions   {{{1

get_files(Pid, FilesBeingWatched) ->
    {ok, AllFiles} = file:list_dir(?LICENSE_FILE_LOCATION),
    LicenseFiles = lists:filter(fun(Filename) -> re:run(Filename, "\.lic$", [{capture, none}]) == match end, AllFiles),
    NewFiles = lists:map(
                 fun(Filename) ->
                         FullPath = filename:join([?LICENSE_FILE_LOCATION, Filename]),
                         {ok, FileInfo} = file:read_file_info(FullPath),
                         {FullPath, FileInfo#file_info.mtime}
                 end, LicenseFiles),
    notify(Pid, NewFiles, FilesBeingWatched == NewFiles).

notify(_Pid, NewFiles, true) ->
    NewFiles;
notify(Pid, NewFiles, false) ->
    Pid ! {update_license_info, NewFiles},
    NewFiles.

%%% Unit Tests   {{{1
-ifdef(EUNIT).
-endif.
