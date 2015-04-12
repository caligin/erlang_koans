-module(erlang_koans_SUITE).
-compile(export_all).

all() ->
    [ can_start
    ].

%% tests

can_start(_Config) ->
    {ok, _apps} = application:ensure_all_started(erlang_koans).

