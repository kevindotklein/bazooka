-module(json_parser).

-export([p_json_null/0, p_json_bool/0, p_json/0, p_json_number/0, p_json_string/0]).

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
  StrParser = bazooka:then_right(
                bazooka:match_char($"),
                bazooka:then_left(bazooka:string_literal(), bazooka:match_char($"))),
  bazooka:fmap(
    StrParser,
    fun(String) -> {json_string, String} end).

p_json() ->
  bazooka:choice([
    p_json_null(),
    p_json_bool(),
    p_json_number()
  ]).