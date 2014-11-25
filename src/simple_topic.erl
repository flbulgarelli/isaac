-module(simple_topic).
-behaviour(gen_fsm).

-record(idle, {spec, subscribers}).
-record(voting, {spec, subscribers, proposal_ref, votes}).

-export([start_link/1,
  subscribe/2,
  unsubscribe/2,
  propose/2,
  vote_for/3,
  status/1,
  init/1 ]).

-export([idle/2, idle/3, voting/2, voting/3, terminate/3]).

start_link(Spec) ->
  gen_fsm:start_link(?MODULE, [Spec], []).

init([Spec]) ->
  {ok, idle, #idle{spec=Spec, subscribers=[]}}.

status(Topic) ->
  gen_fsm:sync_send_event(Topic, status, 500).

subscribe(Subscriber, Topic) ->
  gen_fsm:send_event(Topic, {subscribe, Subscriber}).

unsubscribe(Subscriber, Topic) ->
  gen_fsm:send_event(Topic, {unsubscribe, Subscriber}).

propose(Proposal, Topic) ->
  ProposalRef = make_ref(),
  gen_fsm:send_event(Topic, {propose, Proposal, ProposalRef}),
  ProposalRef.

vote_for(Elector, ProposalRef, Topic) ->
  gen_fsm:send_event(Topic, {vote_for, Elector, ProposalRef}).

idle(status, _, S) ->
  {reply, idle, idle, S}.

idle({subscribe, Sub}, S = #idle{subscribers=Subs}) ->
  {next_state, idle, S#idle{subscribers=[Sub|Subs]}};

idle({unsubscribe, Sub}, S = #idle{subscribers = Subs}) ->
  {next_state, idle, S#idle{subscribers=lists:delete(Sub, Subs)}};

idle(M = {propose, _, ProposalRef}, {idle, Spec={_, Timeout}, Subs}) ->
  notify_proposal(M, Subs),
  gen_fsm:send_event_after(Timeout, timeout),
  {next_state, voting, {voting, Spec, Subs, ProposalRef, dict:new()}}.


voting(status, _, S) ->
  {reply, voting, voting, S}.

voting({vote_for, Elector, ProposalRef}, S = #voting{votes = Votes}) ->
  {next_state, voting, S#voting{votes = dict:store(Elector, 1, Votes)}};

voting(timeout, {voting, Spec={MajorityModel, _}, Subscribers, ProposalRef, Votes}) ->
  notify_result(ProposalRef, MajorityModel, Votes, Subscribers),
  {next_state, idle, {Spec, Subscribers}}.

terminate(_, _, _) -> ok.


votes_count(Votes) ->
  dict:fold(fun(_, V, A) -> V + A end, 0, Votes).

notify_result(ProposalRef, MajorityModel, Votes, Subscribers) ->
  case proposal_approved(MajorityModel, Votes, Subscribers) of
    true -> [S ! {won, ProposalRef} || S <- Subscribers];
    false -> ok
  end.

notify_proposal(Message, Subscribers) ->
  [S ! Message || S <- Subscribers].

proposal_approved(MajorityModel, Votes, Subscribers) ->
  MajorityModel({votes_count(Votes), length(Subscribers)}).



