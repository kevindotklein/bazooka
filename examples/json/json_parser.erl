-module(json_parser).

-export([p_json_null/0,
        p_json_bool/0,
        p_json/0,
        p_json_number/0,
        p_json_string/0,
        p_json_array/0,
        p_json_object/0]).

-type json_value()
  :: json_null
  | {json_bool, boolean()}
  | {json_number, number()}
  | {json_string, string()}
  | {json_array, [json_value()]}
  | {json_object, [{string(), json_value()}]}.

-spec p_json_null() -> bazooka:parser(json_value()).
p_json_null() ->
  bazooka:fmap(bazooka:match_string("null"), fun(_) -> json_null end).

-spec p_json_bool() -> bazooka:parser(json_value()).
p_json_bool() ->
  bazooka:fmap(
    bazooka:choice([bazooka:match_string("true"), bazooka:match_string("false")]),
    fun("true")  -> {json_bool, true};
       ("false") -> {json_bool, false}
    end).

p_json_number() ->
  fun(Input) ->
    case (bazooka:span(fun bazooka:is_digit/1))(Input) of
      {ok, IntPart, Rest1} when IntPart =/= [] ->
        case (bazooka:match_char($.))(Rest1) of
          {ok, _, Rest2} ->
            case (bazooka:span(fun bazooka:is_digit/1))(Rest2) of
              {ok, FracPart, Rest3} when FracPart =/= [] ->
                NumberStr = lists:flatten([IntPart, ".", FracPart]),
                {ok, {json_number, list_to_float(NumberStr)}, Rest3};
              _ ->
                {error, Rest1}
            end;
          {error, _} ->
            {ok, {json_number, list_to_integer(IntPart)}, Rest1}
        end;
      _ -> {error, Input}
    end
  end.

-spec p_json_string() -> bazooka:parser(json_value()).
p_json_string() ->
  StrParser = bazooka:between(bazooka:match_char($"),
                              bazooka:match_char($"),
                              bazooka:string_literal()),
  bazooka:fmap(
    StrParser,
    fun(String) -> {json_string, String} end).

-spec p_json_array() -> bazooka:parser(json_value()).
p_json_array() ->
  Items = bazooka:sep_by(bazooka:then_right(bazooka:spaces(),
    bazooka:then_left(bazooka:match_char($,), bazooka:spaces())), bazooka:lazy(fun p_json/0)),

  Array = bazooka:then_right(bazooka:match_char($[),
    bazooka:then_right(bazooka:spaces(),
      bazooka:then_left(bazooka:then_left(Items, bazooka:spaces()), bazooka:match_char($])))),

  bazooka:fmap(
    Array,
    fun(A) -> {json_array, A} end).

-spec p_json_object() -> bazooka:parser(json_value()).
p_json_object() ->
  Pair = bazooka:applicative(bazooka:applicative(bazooka:fmap(
    bazooka:between(bazooka:match_char($"), bazooka:match_char($"), bazooka:string_literal()),
    fun(Key) ->
      fun(_) ->
        fun(Value) -> {Key, Value} end
      end
    end), bazooka:between(bazooka:spaces(), bazooka:spaces(), bazooka:match_char($:))), bazooka:lazy(fun p_json/0)),

  Items = bazooka:sep_by(
    bazooka:between(bazooka:spaces(), bazooka:spaces(), bazooka:match_char($,)), Pair),

  Obj = bazooka:between(bazooka:match_char(${), bazooka:match_char($}),
    bazooka:between(bazooka:spaces(), bazooka:spaces(), Items)),

  bazooka:fmap(Obj, fun(O) -> {json_object, O} end).

p_json() ->
  bazooka:choice([
    p_json_null(),
    p_json_bool(),
    p_json_number(),
    p_json_string(),
    p_json_array(),
    p_json_object()
  ]).