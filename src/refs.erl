-module(refs).

-export([with_ref/1]).

with_ref(Fun) ->
  Ref = make_ref(),
  Fun(Ref),
  Ref.