-module(isaac_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"topics">>], _Req) ->
    T = topics_server:list_topics(),
    {ok, [], <<T>>};

handle('POST',[<<"topics">>], _Req) ->
    TopicRef = topics_server:start_topic({1000, majority:simple()}),
    {ok, [], <<TopicRef>>};

handle('GET',[<<"topics">>, TopicRef], _Req) ->
    Status = topics_server:get_topic_status(TopicRef),
    {ok, [], <<Status>>};

handle('POST',[<<"topics">>, TopicRef, <<"proposals">>], _Req) ->
    ProposalRef = topics_server:start_topic_proposal({{"name", "Dummy proposal"}}, TopicRef),
    {ok, [], <<ProposalRef>>};

handle('POST',[<<"topics">>, TopicRef, <<"subscribers">>], _Req) ->
    {ok, [], <<"TODO: add subscriber">>};

handle('DELETE',[<<"topics">>, TopicRef, <<"subscribers">>, SubscriberId], _Req) ->
    {ok, [], <<"TODO: remove subscriber">>};


handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.