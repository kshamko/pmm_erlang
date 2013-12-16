-module(task3_auth_worker).
-author("kostik").

-behaviour(gen_fsm).

%% API
-export([start_link/1, authorize/2]).

%% States
-export([opened/2, closed/2, opened/3, closed/3]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).
-define(TTL, 5000).

-record(state, {token, server_pid}).
%%%===================================================================
%%% API
%%%===================================================================
authorize(AppId, Pid) ->
  gen_fsm:sync_send_event(Pid, {authorize, AppId}).

start_link(ServerPid) ->
  gen_fsm:start_link(?MODULE, [ServerPid], [{timeout, ?TTL}]).

%%%===================================================================
%%% States
%%%===================================================================

%% async
opened(timeout, State) ->
  {stop, normal, State};
opened(_Event, State) ->
  {next_state, opened, State}.

closed(_Event, State) ->
  {next_state, closed, State}.

%% sync
opened(_Event, _From, State) ->
  {reply, {error, no_token}, closed, State}.

closed({authorize, AppId}, _From, State) ->
  case get_hash(AppId) of
    {ok, Token} ->
      {reply, {token, Token}, opened, State#state{token = Token}, ?TTL};
    {error, Error} ->
      {reply, {error, Error}, closed, State}
  end;
closed(_Event, _From, State) ->
  {reply, {error, no_token}, closed, State}.

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([ServerPid]) ->
  {ok, closed, #state{server_pid = ServerPid}}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(Info, StateName, State) ->
  io:format("Info ~p~n", [Info]),
  {next_state, StateName, State}.

terminate(_Reason, _StateName, State) ->
  io:format("Terminate auth worker ~p~n", [self()]),
  task3_server:kill_auth_worker(State#state.server_pid, self()),
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_hash(AppId) ->
  case generate_hash_appid(AppId) of
    {ok, Hash} ->
      {ok, PidHash} = generate_hash_pid(),
      {ok, tidy_hash(crypto:hash(md5, lists:concat([Hash, PidHash])))};
    {error, Error} ->
      {error, Error}
  end.

generate_hash_pid() ->
  Hash = crypto:hash(md5, pid_to_list(self())),
  {ok, tidy_hash(Hash)}.

generate_hash_appid(AppId) when is_binary(AppId) ->
  {ok, tidy_hash(crypto:hash(md5, AppId))};
generate_hash_appid(AppId) when is_atom(AppId) ->
  {ok, tidy_hash(crypto:hash(md5, atom_to_binary(AppId, utf8)))};
generate_hash_appid(_AppId) ->
  {error, bad_appid_format}.

tidy_hash(Hash) when is_binary(Hash) ->
  list_to_atom(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash])).
