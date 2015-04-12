-module(koan_engine).
-behaviour(application).
-behaviour(supervisor).

-export([start/2]).
-export([stop/1]).

-export([init/1]).

-export([run/0]).

%% application

start(_StartType, _StartArgs) ->
    register(koan_engine, self()),
    SupervisorRef = supervisor:start_link({local, koan_supervisor}, ?MODULE, []),
    ok = gen_event:add_handler(koan_changes, koan_changes, []),
    SupervisorRef.

stop(_State) ->
    ok.

%% supervisor

init(_) ->
    {ok, {
        { one_for_one, 5, 100 },
        [
            {koan_changes, {gen_event, start_link, [{local, koan_changes}]}, permanent, 5000, worker, [dynamic]},
            {koan_scanner, {koan_scanner, start_link, []}, permanent, 5000, worker, [koan_scanner]}
        ]
    }}.

%% API

run()->
  run_koan("src/01-equalities.eko").

%% private

run_koan(KoanFilename) -> 
    {ok, Bin} = file:read_file(KoanFilename),
    Content = binary_to_list(Bin),
    {ok, Tokens, _End} = erl_scan:string(Content),
    Statements = tokens_to_statements(Tokens),
    ParsedStatements = [ Parsed || {ok, Parsed} <- lists:map(fun erl_parse:parse_exprs/1, Statements)],
    Koans = retain_koans(ParsedStatements),
    execute_koans(Koans).

tokens_to_statements(Tokens) ->
  tokens_to_statements([],[],Tokens).

tokens_to_statements(StatementsReversed, _CurrentKoanTokens, []) ->
  lists:reverse(StatementsReversed);
tokens_to_statements(StatementsReversed, CurrentStatementTokensReversed, [{dot, _} = Dot | MoreTokens]) ->
  tokens_to_statements([lists:reverse(CurrentStatementTokensReversed, [Dot]) | StatementsReversed], [], MoreTokens);
tokens_to_statements(StatementsReversed, CurrentStatementTokensReversed, [NextToken | MoreTokens]) ->
  tokens_to_statements(StatementsReversed, [NextToken | CurrentStatementTokensReversed], MoreTokens).

retain_koans(Statements) ->
    [ {Meditation, MeditationTerm} || [{tuple, _, [{string, _, Meditation}, MeditationTerm]}] <- Statements].

execute_koans([]) ->
  ok;
execute_koans([H | T]) ->
  case execute_koan(H) of
    ok -> execute_koans(T);
    {error, _Whatever} -> ok
  end.

execute_koan({_Meditation, Expression} = Koan) ->
  case has_blanks(Expression) of
    true -> 
      print_koan_failure("Fill the blanks in your knowledge", Koan),
      {error, blanks};
    false ->
      case eval_koan_expr(Expression) of
        {ok, _} -> ok;
        {error, Reason} = Failure -> print_koan_failure(Reason, Koan), Failure
      end
    end.


%% From http://www.erlang.org/doc/apps/erts/absform.html
has_blanks({integer, _, _}) ->
  false;
has_blanks({float, _, _}) ->
  false;
has_blanks({string, _, _}) ->
  false;
has_blanks({atom, _, _}) ->
  false;
has_blanks({match,_,Pattern,Expression}) ->
  has_blanks(Pattern) orelse has_blanks(Expression);
has_blanks([]) ->
  false;
has_blanks([H | T]) ->
  case has_blanks(H) of
    true -> true;
    false -> has_blanks(T)
  end;
has_blanks({var,_,'_'}) ->
  false; %% this is the catchall so it's permitted
has_blanks({var,_,VarAtom}) ->
  lists:all(fun(C) -> C =:= $_ end, atom_to_list(VarAtom));
has_blanks({tuple, _, TupleElements}) ->
  has_blanks(TupleElements);
has_blanks({nil,_}) ->
  false;
has_blanks({cons, _,Consed,Consee}) ->
  has_blanks(Consed) orelse has_blanks(Consee);
  %%ignoring binaries looks too difficoult
has_blanks({op,_,_Op,Lhs,Rhs}) ->
  has_blanks(Lhs) orelse has_blanks(Rhs);
has_blanks({op,_,_Op,Expression}) ->
  has_blanks(Expression);
has_blanks(_Expression) ->
  false.

print_koan_failure(FailureMessage, {Meditation, Expression}) -> 
  io:format("Now meditate:~n",[]), %todo: filename + line would be handy
  io:format(Meditation ++ "~n",[]),
  io:format("~p~n",[FailureMessage]),
  io:format("~s~n",[erl_pp:expr(Expression)]). %% TODO: convert in reasonable erlang

eval_koan_expr(Expression) ->
  try erl_eval:exprs([Expression], []) of
    {value, Value, _Boh} -> {ok, Value}
  catch
    error:Reason -> {error, Reason}
  end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

tokens_of(String) ->
  {ok, Tokens, _End} = erl_scan:string(String),
  Tokens.

tokens_to_statements_test_() ->
  [ {"a statement up to dot is a single element", ?_assertEqual(1, length(tokens_to_statements(tokens_of("an_atom."))))},
    {"a tuple is a statement", ?_assertEqual(1, length(tokens_to_statements(tokens_of("{a, tuple}."))))},
    {"two tuples separated by dot are two statements", ?_assertEqual(2, length(tokens_to_statements(tokens_of("{a, tuple}. {another, tuple}."))))},
    {"after last dot stuff not terminated is dropped", ?_assertEqual(1, length(tokens_to_statements(tokens_of("{a, tuple}. no dot after this"))))}
  ].

parse_string(String) ->
  Tokens = tokens_of(String),
  {ok, Parsed} = erl_parse:parse_exprs(Tokens),
  Parsed.

retain_koans_test_() ->
  [ {"a tuple of string, other is retained", ?_test([_Something] = retain_koans([parse_string("{\"str\", atom}.")]))},
    {"a tuple of three+ is not retained", ?_test([] = retain_koans([parse_string("{\"str\", atom, another_atom}.")]))},
    {"a list is not retained", ?_test([] = retain_koans([parse_string("[\"str\", atom].")]))},
    {"a tuple of non-string, other is not retained", ?_test([] = retain_koans([parse_string("{atom, other_atom}.")]))}
  ].

has_blanks_test_() ->
  [ {"an integer has no blanks", ?_assertNot(has_blanks(parse_string("1.")))},
    {"a float has no blanks", ?_assertNot(has_blanks(parse_string("1.2.")))},
    {"a string has no blanks", ?_assertNot(has_blanks(parse_string("\"str\".")))},
    {"an atom has no blanks", ?_assertNot(has_blanks(parse_string("atom.")))},
    {"a variable has no blanks when has non _ characters", ?_assertNot(has_blanks(parse_string("Var_.")))},
    {"a variable has no blanks when is the catchall pattern", ?_assertNot(has_blanks(parse_string("_.")))},
    {"a variable has blanks when contains only and more than one _", ?_assert(has_blanks(parse_string("___.")))},
    {"a match has blanks when one of the sides has blanks", ?_assert(has_blanks(parse_string("___ = 1.")))},
    {"a match has no blanks when none one of the sides has blanks", ?_assertNot(has_blanks(parse_string("1 = 2.")))}
  ].

-endif.