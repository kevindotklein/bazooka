-module(bazooka).

-export([match_char/1, match_string/1, fmap/2, bind/2, many/1, choice/1, then/2, pure/1, applicative/2, span/1]).
-export_type([parser/1]).

-type parser(ValueType) ::
  fun((string()) -> {ok, ValueType, string()} | {error, string()}).

%% match character
-spec match_char(char()) -> parser(char()).
match_char(Char) ->
  fun ([C | Rest]) when C == Char ->
        {ok, C, Rest};
      (Input) ->
        {error, Input}
  end.

%% match string
-spec match_string([char()]) -> parser(string()).
match_string([]) ->
  fun(Input) -> {ok, [], Input} end;
match_string([H | T]) ->
  P1 = match_char(H),
  P2 = match_string(T),
  fun(Input) ->
     case P1(Input) of
       {ok, H, Rest1} ->
         case P2(Rest1) of
           {ok, T2, Rest2} -> {ok, [H | T2], Rest2};
           {error, _} = E -> E
         end;
       {error, _} = E -> E
     end
  end.

%% <$>
-spec fmap(parser(A), fun((A) -> any())) -> parser(any()).
fmap(Parser, Fun) ->
  fun(Input) ->
     case Parser(Input) of
       {ok, Val, Rest} -> {ok, Fun(Val), Rest};
       {error, _} = E -> E
     end
  end.

%% >>=
-spec bind(parser(A), fun((A) -> parser(A))) -> parser(A).
bind(P1, Fun) ->
  fun(Input) ->
     case P1(Input) of
       {ok, Val1, Rest1} ->
         P2 = Fun(Val1),
         P2(Rest1);
       {error, _} = E -> E
     end
  end.

%% >>
-spec then(parser(any()), parser(any())) -> parser(any()).
then(P1, P2) ->
  fun(Input) ->
     case P1(Input) of
       {ok, _, Rest} -> P2(Rest);
       {error, _} = E -> E
     end
  end.

%% many parser
-spec many(parser(A)) -> parser([A]).
many(P) ->
  fun(Input) -> many_loop(P, Input, []) end.

many_loop(P, Input, Acc) ->
  case P(Input) of
    {ok, Val, Rest} ->
      many_loop(P, Rest, [Val | Acc]);
    {error, _} ->
      {ok, lists:reverse(Acc), Input}
  end.

%% <|>
-spec choice([parser(any())]) -> parser(any()).
choice([]) ->
  fun(Input) -> {error, Input} end;
choice([P | Ps]) ->
  fun(Input) ->
     case P(Input) of
       {ok, _, _} = Ok -> Ok;
       {error, _} -> (choice(Ps))(Input)
     end
  end.

%% pure
-spec pure(A) -> parser(A).
pure(X) ->
  fun(Input) ->
    {ok, X, Input}
  end.

%% <*>
-spec applicative(parser(fun((A) -> any())), parser(A)) -> parser(any()).
applicative(P1, P2) ->
  fun(Input) ->
    case P1(Input) of
      {ok, Fun, Rest1} ->
        case P2(Rest1) of
          {ok, Val, Rest2} -> {ok, Fun(Val), Rest2};
          {error, _} = E -> E
        end;
      {error, _} = E -> E
    end
  end.

%% span
-spec span(fun((char()) -> boolean())) -> parser(string()).
span(Criteria) ->
  fun(Input) -> span_loop(Criteria, Input, []) end.

span_loop(Criteria, [H|T], Acc) ->
  case Criteria(H) of
    true -> span_loop(Criteria, T, [H | Acc]);
    false -> {ok, lists:reverse(Acc), [H|T]}
  end.