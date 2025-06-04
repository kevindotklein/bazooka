-module(json_parser).

-export([p_json_null/0, p_json_bool/0, p_json/0, p_json_number/0]).

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
  io:format("todo~n").

p_json() ->
  bazooka:choice([
    p_json_null(),
    p_json_bool()
  ]).