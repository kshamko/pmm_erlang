-module(task3_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  MaxRestart = 1,
  MaxTime = 3600,
  {
    ok,
    {
      {one_for_all, MaxRestart, MaxTime},
      [
        {task3_server, {task3_server, start_link, [self()]}, transient, 60000, worker, [task3_server]}
      ]
    }
  }.