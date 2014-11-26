-module(topics_server).
-behaviour(gen_server).

-record(state, {topics, proposal_counter, topic_counter}).

-export([
  start_link/0,
  list_topics/0,
  start_topic/1,
  get_topic_status/1,
  start_topic_proposal/2,
  vote_for_proposal/3,
  subscribe_to_topic/2,
  unsubscribe_to_topic/2
]).

-export([init/1, handle_call/3  ]).

start_link() ->
  gen_server:start_link({local, topics_server}, ?MODULE, [], []).

list_topics() ->
  gen_server:call(topics_server, list_topics, 500).

get_topic_status(TopicRef) ->
  gen_server:call(topics_server, {get_topic_status, TopicRef}, 500).

start_topic(Spec) ->
  gen_server:call(topics_server, {start_topic, Spec}, 100).

start_topic_proposal(Proposal, TopicRef) ->
  gen_server:call(topics_server, {start_topic_proposal, Proposal, TopicRef}, 100).

subscribe_to_topic(Subscriber, TopicRef)->
  gen_server:call(topics_server, {subscribe_to_topic, Subscriber, TopicRef}, 100).

unsubscribe_to_topic(Subscriber, TopicRef)->
  gen_server:cast(topics_server, {unsubscribe_to_topic, Subscriber, TopicRef}).


vote_for_proposal(Elector, ProposalRef, TopicRef) ->
  gen_server:cast(topics_server, {vote_for_proposal, Elector, ProposalRef, TopicRef}).

init(_) ->
  {ok, #state{topics=dict:new(), proposal_counter=0, topic_counter=0}}.

handle_call(list_topics, _, S = #state{topics=Ts}) ->
  {reply, dict:fetch_keys(Ts), S};

handle_call({get_topic_status, TopicRef}, _, S) ->
  case find_topic(TopicRef, S) of
    {ok, Topic} -> {reply, simple_topic:satus(Topic), S};
    _ -> {reply, error, S}
  end;

%%TODO add topic to supervisor hierarchy
handle_call({start_topic, Spec}, _, S = #state{topics=Ts}) ->
  TopicRef = S#state.topic_counter + 1,
  Topic = simple_topic:start_link(Spec),
  {reply, TopicRef, S#state{topic_counter = TopicRef, topics = dict:store(TopicRef, Topic, Ts)}};

%%TODO reply as soon as  end
handle_call({start_topic_proposal, Proposal, TopicRef}, _, S = #state{topics=Ts}) ->
  ProposalRef = S#state.proposal_counter + 1,
  case find_topic(TopicRef, S) of
    {ok, Topic} -> simple_topic:propose(ProposalRef, Proposal, Topic);
    _ -> ok
  end,
  {reply, ProposalRef, S#state{proposal_counter=ProposalRef}}.

find_topic(TopicRef, #state{topics=Ts}) ->
  dict:find(TopicRef, Ts).


update_counter(Name, Ts) ->
  Ts1 = dict:update_counter(Name, 1, Ts),
  {dict:fetch(Name, Ts1), Ts1}.

