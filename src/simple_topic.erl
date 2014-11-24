-module(simple_topic).

-compile(export_all).

start_link(Spec) ->
  spawn_link(?MODULE, init, [Spec]).

init(Spec) ->
  idle({Spec, []}).

status(Topic) ->
  Ref = erlang:monitor(process, Topic),
  Topic ! {self(), Ref, status},
  Status = receive
    {Ref, S} -> S;
    {'DOWN', Ref, _, _, _} -> down
  after 500 ->
    timeout
  end,
  erlang:demonitor(Ref, [flush]),
  Status.

subscribe(Subscriber, Topic)->
  Topic ! {Subscriber, subscribe}.

unsubscribe(Subscriber, Topic)->
  Topic ! {Subscriber, unsubscribe}.

propose(Proposal, Topic) ->
  Ref = make_ref(),
  Topic ! {Ref, propose, Proposal},
  Ref.

vote_for(Elector, Ref, Topic) ->
  Topic ! {Elector, Ref, vote_for}.

idle(S={Spec={_, VotingTimeout}, Subscribers}) ->
  receive
    {Pid, Ref, status} ->
      Pid ! {Ref, idle},
      idle(S);
    {Pid, subscribe} ->
      idle({Spec, [Pid|Subscribers]});
    {Pid, unsubscribe} ->
      idle({Spec, lists:delete(Pid, Subscribers)});
    M = {Ref, propose, _} ->
      notify_proposal(M, Subscribers),
      timer:send_after(VotingTimeout, self(), timeout),
      voting({Spec, Subscribers, Ref, dict:new()})
  end.

voting(S = {Spec={MajorityModel, _}, Subscribers, ProposalRef, Votes}) ->
  receive
    {Pid, Ref, status} ->
      Pid ! {Ref, voting},
      voting(S);
    {Pid, ProposalRef, vote_for} ->
      voting({Spec, Subscribers, ProposalRef, dict:store(Pid, 1, Votes)});
    timeout ->
      notify_result(ProposalRef, MajorityModel, Votes, Subscribers),
      idle({Spec, Subscribers})
  end.

votes_count(Votes) ->
  dict:fold(fun(_, V, A) -> V + A end, 0, Votes).

notify_result(ProposalRef, MajorityModel, Votes, Subscribers) ->
  case proposal_approved(MajorityModel, Votes, Subscribers) of
    true -> [S ! {ProposalRef, won} || S <- Subscribers];
    false -> ok
  end.

notify_proposal(Message, Subscribers) ->
  [S ! Message || S <- Subscribers].

proposal_approved(MajorityModel, Votes, Subscribers) ->
  MajorityModel({votes_count(Votes), length(Subscribers)}).