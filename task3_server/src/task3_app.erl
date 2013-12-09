-module(task3_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Supervisor = task3_sup:start_link(),

    ServerPid = whereis(task3_server),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", task3_http_handler, [{auth_server_pid, ServerPid}]}
        ]}
    ]),
    Port = 8008,
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
      {env, [{dispatch, Dispatch}]}
    ]),

    Supervisor.

stop(_State) ->
    ok.
