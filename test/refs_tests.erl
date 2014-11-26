-module(refs_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

with_ref_answers_the_reference_test() ->
  R = refs:with_ref(fun(_) ->
    1
  end),
  ?assertEqual(R, R).

with_ref_creates_refs_test() ->
  R = refs:with_ref(fun(R) ->
    R
  end),
  ?assert(is_reference(R)).