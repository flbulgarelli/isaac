-module(isaac_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"topics">>], _Req) ->
    T = topics_server:list_topics(),
    {ok, [], jiffy:encode({[
      {topic_ids, T}
    ]})};

handle('POST',[<<"topics">>], _Req) ->
    TopicRef = topics_server:start_topic({1000, majority:simple()}),
    {ok, [], jiffy:encode({[
      {topic_id, TopicRef}
    ]})};

handle('GET',[<<"topics">>, TopicRef], _Req) ->
    Status = topics_server:get_topic_status(TopicRef),
    {ok, [], jiffy:encode({[
      {topic_id, TopicRef},
      {status, Status}
    ]})};

handle('POST',[<<"topics">>, TopicRef, <<"proposals">>], _Req) ->
    ProposalRef = topics_server:start_topic_proposal({{"name", "Dummy proposal"}}, TopicRef),
    {ok, [], jiffy:encode({[
      {topic_id, TopicRef},
      {proposal_id, ProposalRef}
    ]})};

handle('POST',[<<"topics">>, TopicRef, <<"proposals">>, ProposalRef, <<"votes">>], _Req) ->
    topics_server:vote_for_proposal(null, ProposalRef, TopicRef),
    {ok, [], <<>>};

% TODO get from session/token
handle('POST',[<<"topics">>, TopicRef, <<"subscribers">>], _Req) ->
    SubscriberId = topics_server:subscribe_to_topic(null, TopicRef),
    {ok, [], jiffy:encode({[
      {topic_id, TopicRef},
      {subscriber_id, SubscriberId}
    ]})};

handle('DELETE',[<<"topics">>, TopicRef, <<"subscribers">>, SubscriberId], _Req) ->
    topics_server:unsubscribe_to_topic(null, TopicRef),
    {ok, [], <<>>};


handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.