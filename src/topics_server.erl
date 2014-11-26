-module(topics_server).
-behaviour(gen_server).

-record(state, {topics}).

-export([
  start_link/0,
  list_topics/1,
  start_topic/2,
  get_topic_status/2,
  start_topic_proposal/3
 %% subscribe_to_topic/2, unsubscribe_to_topic/2
]).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

list_topics(Server) ->
  gen_server:call(Server, list_topics, 500).

get_topic_status(TopicRef, Server) ->
  gen_server:call(Server, {get_topic_status, TopicRef}, 500).

start_topic(Spec, Server) ->
  refs:with_ref(fun(TopicRef) ->
    gen_server:cast(Server, {start_topic, TopicRef, Spec}, 500)
  end).

start_topic_proposal(Proposal, TopicRef, Server) ->
  refs:with_ref(fun(ProposalRef, TopicRef) ->
    gen_server:cast(Server, {start_topic_proposal, ProposalRef, Proposal, TopicRef}, 100)
  end).

init([]) ->
  {ok, #state{topics=dict:new()}}.

handle_call(list_topics, _, S = #state{topics=Ts}) ->
  {reply, dict:fetch_keys(Ts), S};

handle_call({get_topic_status, TopicRef}, _, S) ->
  case find_topic(TopicRef, S) of
    {ok, Topic} -> {reply, simple_topic:satus(Topic), S};
    _ -> {reply, error, S}
  end.

handle_cast({start_topic, TopicRef, Spec}, S = #state{topics=Ts}) ->
  Topic = simple_topic:start_link(Spec),
  {noreply, S#state{topics = dict:store(TopicRef, Topic, Ts)}};

handle_cast({start_topic_proposal, ProposalRef, Proposal, TopicRef}, S) ->
  case find_topic(TopicRef, S) of
    {ok, Topic} -> simple_topic:propose(ProposalRef, Proposal, Topic);
    _ -> ok
  end,
  {noreply, S}.

find_topic(TopicRef, S = #state{topics=Ts}) ->
  dict:find(TopicRef, Ts).
