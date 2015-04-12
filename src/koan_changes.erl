-module(koan_changes).
-behaviour(gen_event).

-export([init/1]).
-export([handle_event/2]).
-export([handle_info/2]).
-export([handle_call/2]).
-export([code_change/3]).
-export([terminate/2]).

%% gen_event

init(_) ->
    {ok, []}.

handle_event(koan_change, State) ->
    koan_engine:run(),
    {ok , State}.

handle_info(_Msg, State) ->
    {ok , State}.

handle_call(_Req, State) ->
    {ok, {error, badrequest}, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.
