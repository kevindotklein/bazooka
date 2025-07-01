-module(bazooka).

-export([match_char/1,
        match_string/1,
        fmap/2,
        bind/2,
        many/1,
        choice/1,
        then/2,
        pure/1,
        applicative/2,
        span/1,
        is_digit/1,
        is_dot/1,
        bool_choice/1,
        then_right/2,
        then_left/2,
        string_literal/0,
        spaces/0,
        sep_by/2,
        lazy/1]).
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

span_loop(_, [], Acc) -> {ok, lists:reverse(Acc), ""};
span_loop(Criteria, [H|T], Acc) ->
  case Criteria(H) of
    true -> span_loop(Criteria, T, [H | Acc]);
    false -> {ok, lists:reverse(Acc), [H|T]}
  end.

-spec is_digit(char()) -> boolean().
is_digit(Char) when Char >= $0, Char =< $9 -> true;
is_digit(_) -> false.

-spec is_dot(char()) -> boolean().
is_dot(Char) when Char == $. -> true;
is_dot(_) -> false.

%% u can use lists:any/2 :p
-spec bool_choice([fun((A :: char()) -> boolean())]) -> fun((A :: char()) -> boolean()).
bool_choice([BoolFun]) -> fun(Char) -> BoolFun(Char) end;
bool_choice([BoolFun|BoolFuns]) ->
  fun(Char) ->
    case BoolFun(Char) of
      true -> true;
      false -> (bool_choice(BoolFuns))(Char)
    end
  end.

%% *>
-spec then_right(parser(A), parser(B)) -> parser(A | B).
then_right(P1, P2) ->
  fun(Input) ->
    case P1(Input) of
      {ok, _, Rest} -> P2(Rest);
      {error, _} = E -> E
    end
  end.

%% <*
-spec then_left(parser(A), parser(B)) -> parser(A | B).
then_left(P1, P2) ->
  fun(Input) ->
    case P1(Input) of
      {ok, Val, Rest1} -> 
        case P2(Rest1) of
          {ok, _, Rest2} -> {ok, Val, Rest2};
          {error, _} = E -> E
        end;
      {error, _} = E -> E
    end
  end.

%% TODO: escape chars
-spec string_literal() -> parser(string()).
string_literal() ->
  span(fun(Char) ->
           case Char of
               $" -> false;
               _  -> true
           end
       end).

-spec spaces() -> parser(string()).
spaces() ->
  span(fun(Char) ->
           case Char of
            32 -> true;
            _ -> false
           end
    end).

-spec lazy(fun((A :: string() | char()) -> (parser(any())))) -> parser(any()).
lazy(Fun) ->
  fun(Input) -> (Fun())(Input) end.

%% P1 = sep, P2 = element
-spec sep_by(parser(A :: any()), parser(B)) -> parser([B]).
sep_by(P1, P2) ->
  choice([
    applicative(fmap(P2, fun(First) ->
        fun(Rest) -> [First | Rest] end
      end), many(then_right(P1, P2))),
    pure([])]).