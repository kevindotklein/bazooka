-module(bazooka_tests).
-include_lib("eunit/include/eunit.hrl").

match_char_test() ->
  MatchA = bazooka:match_char($a),
  ?assertEqual({ok, $a, "b"}, MatchA("ab")).

match_string_test() ->
  MatchStr = bazooka:match_string("abc"),
  ?assertEqual({ok, "abc", "def"}, MatchStr("abcdef")).

fmap_test() ->
  MatchA = bazooka:match_char($a),
  Fmap = bazooka:fmap(MatchA, fun(A) -> {letter, A} end),
  ?assertEqual({ok, {letter, $a}, "b"}, Fmap("ab")).

bind_test() ->
  MatchAA = bazooka:match_string("aa"),
  Bind = bazooka:bind(MatchAA, fun bazooka:match_string/1),
  ?assertEqual({ok, "aa", "b"}, Bind("aaaab")).

then_test() ->
  MatchA = bazooka:match_char($a),
  MatchB = bazooka:match_char($b),
  Then = bazooka:then(MatchA, MatchB),
  ?assertEqual({ok, $b, "c"}, Then("abc")).

many_test() ->
  MatchA = bazooka:match_char($a),
  Many = bazooka:many(MatchA),
  ?assertEqual({ok, "aaa", "b"}, Many("aaab")).

choice_test() ->
  MatchA = bazooka:match_char($a),
  MatchB = bazooka:match_char($b),
  Choice = bazooka:choice([MatchA, MatchB]),
  ?assertEqual({ok, $a, "c"}, Choice("ac")),
  ?assertEqual({ok, $b, "c"}, Choice("bc")).