-module(majority_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

simple_approved_border_test() ->
  Model = majority:simple(),
  ?assert(Model({1, 1})).

simple_approved_test() ->
  Model = majority:simple(),
  ?assert(Model({51, 100})).

simple_rejected_test() ->
  Model = majority:simple(),
  ?assert(not Model({0, 1})).