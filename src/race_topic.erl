-module(race_topic).

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
      running({Spec, Subscribers, init_votes_by_proposal(Ref)})
  end.

running(S = {Spec={MajorityModel, VotingTimeout}, Subscribers, VotesByProposal}) ->
  receive
    {Pid, Ref, status} ->
      Pid ! {Ref, running},
      running(S);
    {Pid, ProposalRef, vote_for} ->
      running({Spec, Subscribers, add_vote_for(Pid, ProposalRef, VotesByProposal)});
    M = {OtherProposalRef, propose, _} ->
      notify_proposal(M, Subscribers),
      timer:send_after(VotingTimeout, self(), timeout),
      running({Spec, Subscribers, add_proposal(OtherProposalRef, VotesByProposal)});
    timeout ->
      notify_result(MajorityModel, VotesByProposal, Subscribers),
      idle({Spec, Subscribers})
  end.

init_votes_by_proposal(Ref) ->
  dict:from_list([{Ref, dict:new()}]).

add_proposal(ProposalRef, VotesByProposal) ->
  dict:store(ProposalRef, dict:new(), VotesByProposal).

add_vote_for(Elector, ProposalRef, VotesByProposal) ->
  Votes = dict:find(ProposalRef, VotesByProposal),
  dict:store(ProposalRef, dict:store(Elector, 1, Votes)).

votes_count(VotesByProposal) ->
  dict:map(fun(_, Votes) ->
    dict:fold(fun(_, V, A) -> V + A end, 0, Votes) end, VotesByProposal).

notify_result(MajorityModel, VotesByProposal, Subscribers) ->
  ProposalRef = approved_proposal(MajorityModel, VotesByProposal),
  [S ! {ProposalRef, won} || S <- Subscribers].

notify_proposal(Message, Subscribers) ->
  [S ! Message || S <- Subscribers].

approved_proposal(MajorityModel, Votes) ->
  MajorityModel(votes_count(Votes)).