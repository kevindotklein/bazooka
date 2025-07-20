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

pure_test() ->
  Pure = bazooka:pure("zzz"),
  ?assertEqual({ok, "zzz", "aaa"}, Pure("aaa")).

applicative_test() ->
  Match1 = bazooka:match_char($1),
  Match2 = bazooka:match_char($2),
  Fmap = bazooka:fmap(Match1, fun(N1) -> fun(N2) -> (N1 - $0) + (N2 - $0) end end),
  Applicative = bazooka:applicative(Fmap, Match2),
  ?assertEqual({ok, 3, ""}, Applicative("12")).