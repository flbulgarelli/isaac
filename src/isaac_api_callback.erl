-module(isaac_api_callback).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
    handle(Req#req.method, elli_request:path(Req), Req).


handle('GET',[<<"topics">>], _Req) ->
    {ok, [], <<"TODO: list topics">>};

handle('POST',[<<"topics">>], _Req) ->
    {ok, [], <<"TODO: create topic">>};

handle('GET',[<<"topics">>, TopicId], _Req) ->
    {ok, [], <<"TODO: get topic status">>};

handle('POST',[<<"topics">>, TopicId, <<"proposals">>], _Req) ->
    {ok, [], <<"TODO: create proposal">>};

handle('POST',[<<"topics">>, TopicId, <<"subscribers">>], _Req) ->
    {ok, [], <<"TODO: add subscriber">>};

handle('DELETE',[<<"topics">>, TopicId, <<"subscribers">>, SubscriberId], _Req) ->
    {ok, [], <<"TODO: remove subscriber">>};


handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

handle_event(_Event, _Data, _Args) ->
    ok.