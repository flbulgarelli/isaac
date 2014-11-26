-module(isaac_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ElliOpts = [{callback, isaac_api_callback}, {port, 4949}],
    ElliSpec = {
        isaac_http,
        {elli, start_link, [ElliOpts]},
        permanent,
        5000,
        worker,
        [elli]},
    TopicsServerSpec = {
        topics_server,
        {topics_server, start_link, []},
        permanent,
        brutal_kill,
        worker,
        [topics_server]
    },
    {ok, { {one_for_one, 5, 10}, [ElliSpec, TopicsServerSpec]} }.