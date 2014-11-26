-module(isaac_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() -> application:start(isaac).

start(_StartType, _StartArgs) ->
    isaac_sup:start_link().

stop(_State) ->
    ok.
