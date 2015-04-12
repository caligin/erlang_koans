-module(koan_scanner).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
    times = [] :: [{string(), file:date_time()}]
}).

-define(SCAN_INTERVAL, 3000).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link(?MODULE, [], []).

%% gen_server.

init([]) ->
    gen_server:cast(self(), scan),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(scan, State) ->
    NewTimes = koan_times(),
    case changes_present(State#state.times, NewTimes) of
        true -> gen_event:notify(koan_changes, koan_change);
        false -> ok        
    end,
    timer:apply_after(?SCAN_INTERVAL, gen_server, cast, [self(), scan]),
    {noreply, State#state{ times = NewTimes }};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% private

koan_times() ->
    {ok, Files} = file:list_dir("src"),
    [ {F, last_write_of(F)} || F <- Files, is_koan(F)].

is_koan(File) ->
    re:run(File, "^\\d\\d-\\w+\\.eko$", [{capture, none}]) =:= match.    

last_write_of(File) ->
    {ok, Info} = file:read_file_info("src/" ++ File),
    Info#file_info.mtime.

changes_present([], []) ->
    false;
changes_present(PreviousTimes, [ {File, NewTime} | OtherNewTimes]) ->
    case proplists:lookup(File, PreviousTimes) of
        none -> true;
        {File, OldTime} -> OldTime < NewTime orelse changes_present(proplists:delete(File, PreviousTimes), OtherNewTimes)
    end;
changes_present([_AtLeastOne | _Others], []) ->
    true.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


changes_test_() ->
  Past = {{1,1,1}, {1,1,1}},
  Present = {{1,1,1}, {1,1,2}},
  File = "bla.eko",
  GenericEntry = {File, Present},
  [ {"two emply lists has no changes", ?_assertNot(changes_present([],[]))},
    {"has changes when new element", ?_assert(changes_present([],[GenericEntry]))},
    {"has changes when deleted element", ?_assert(changes_present([GenericEntry], []))},
    {"has changes when existent element has more recent modify date", ?_assert(changes_present([{File, Past}], [{File, Present}]))},
    {"has no changes when all entries were known and dates are not newer", ?_assertNot(changes_present([GenericEntry], [GenericEntry]))}
  ].


-endif.
