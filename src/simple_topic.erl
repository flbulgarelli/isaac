-module(simple_topic).

-compile(export_all).

start_link(Spec) ->
  spawn_link(?MODULE, init, [Spec]).

init(Spec) ->
  idle({Spec, []}).

idle({Spec={_, VotingTimeout}, Subscribers}) ->
  receive
    {Pid, subscribe} ->
      idle({Spec, [Pid|Subscribers]});
    {Pid, unsubscribe} ->
      idle({Spec, lists:delete(Pid, Subscribers)});
    M = {Ref, propose, _} ->
      notify_proposal(M, Subscribers),
      timer:send_after(VotingTimeout, self(), timeout),
      voting({Spec, Subscribers, Ref, dict:new()})
  end.

voting({Spec={MajorityModel, _}, Subscribers, Ref, Votes}) ->
  receive
    {Pid, Ref, vote_for} ->
      voting({Spec, Subscribers, Ref, dict:store(Pid, 1, Votes)});
    timeout ->
      notify_result(Ref, MajorityModel, Votes, Subscribers),
      idle({Spec, Subscribers})
  end.

votes_count(Votes) ->
  dict:fold(fun(_, V, A) -> V + A end, 0, Votes).

notify_result(Ref, MajorityModel, Votes, Subscribers) ->
  case proposal_approved(MajorityModel, Votes, Subscribers) of
    true -> [S ! {Ref, won} || S <- Subscribers]
  end.

notify_proposal(Message, Subscribers) ->
  [S ! Message || S <- Subscribers].

proposal_approved(MajorityModel, Votes, Subscribers) ->
  MajorityModel(votes_count(Votes), lists:size(Subscribers)).