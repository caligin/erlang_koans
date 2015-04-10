-module('01-equalities').
%-export([meditations/0]).
-compile(export_all).

-define(KOAN(Meditation, Facts, Expectation),
  case Facts of
    Expectation -> ok
  end).

run()->
  run_koan("src/01-equalities.eko").


evalisevil(String) ->
  {ok, Tokens, _End} = erl_scan:string(String),
  {ok, ExprList} = erl_parse:parse_exprs(Tokens),
  try erl_eval:exprs(ExprList, [{'__', 'fill in the blank'}]) of
    {value, Value, _Boh} -> io:format("asd~n",[]), Value;
    Other -> io:format("que~n",[]), Other
  catch
    error:Reason -> Reason
  end.


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
  %% this means trailing tokens after last dot are discarded
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

execute_koan({Meditation, Expression} = Koan) ->
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

