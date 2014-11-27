-module(isaac_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    case is_authorized(Req) of
      true  -> handle(Req#req.method, elli_request:path(Req), Req);
      false -> {401, [], <<>>}
    end.


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

is_authorized(Req) ->
    case get_access_token(Req) of
        {ok, Token} ->
            case oauth2:verify_access_token(Token, []) of
                {ok, _Identity} -> true;
                {error, access_denied} -> false
            end;
        {error, _} -> false
    end.

get_access_token(Req) ->
    case elli:header(<<"authorization">>, Req) of
        {<<"Bearer ", Token/binary>>, _Req} ->
            {ok, Token};
        _ ->
            case elli:qs_val(<<"access_token">>, Req) of
                {Token, _Req} ->
                    {ok, Token};
                _ ->
                    {error, missing}
            end
    end.