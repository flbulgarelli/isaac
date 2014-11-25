-module(simple_topic_tests).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

workflow_test() ->
  {ok, Topic} = simple_topic:start_link({ majority:simple(), 900 }),
  simple_topic:subscribe(self(), Topic),
  ProposalRef = simple_topic:propose({}, Topic),
  receive
    {propose, _, ProposalRef } -> ok
  after 500 ->
    ?assert(false)
  end,
  simple_topic:vote_for(self(), ProposalRef, Topic),
  receive
    {won, ProposalRef} -> ok
  after 2000 ->
    ?assert(false)
  end.

topic_start_idle_test() ->
  {ok, Topic} = simple_topic:start_link({ majority:simple(), 100 }),
  ?assertEqual(idle, simple_topic:status(Topic)).

topic_becomes_voting_with_proposal_test() ->
  {ok, Topic} = simple_topic:start_link({ majority:simple(), 100 }),
  simple_topic:subscribe(self(), Topic),
  simple_topic:propose({}, Topic),
  ?assertEqual(voting, simple_topic:status(Topic)).

topic_becomes_idle_after_voting_test() ->
  {ok, Topic} = simple_topic:start_link({ majority:simple(), 100 }),
  simple_topic:subscribe(self(), Topic),
  simple_topic:propose({}, Topic),
  timer:sleep(200),
  ?assertEqual(idle, simple_topic:status(Topic)).

only_subscribers_can_vote_test() ->
  ?assert(false).

only_subscribers_can_propose_test() ->
  ?assert(false).
