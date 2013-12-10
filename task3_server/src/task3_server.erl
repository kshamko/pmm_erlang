-module(task3_server).
-author("konstantin.shamko@gmail.com").

-behaviour(gen_server).

%% API
-export([start_link/1, process_request/4, kill_auth_worker/2]).

%% gen_server callbacks
-export([init/1, code_change/3, handle_call/3, terminate/2, handle_cast/2]).

-define(SERVER, ?MODULE).
-define(AUTH_WORKER(WorkerName, ServerPid),
  {WorkerName,
    {task3_auth_worker, start_link, [ServerPid]},
    temporary,
    120000,
    worker,
    [task3_auth_worker]}).


-record(state, {fsms, tokens, supervisor}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(SupPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {SupPid}, []).

process_request(Method, auth, Params, Pid) ->
  gen_server:call(Pid, {Method, auth, Params});
process_request(Method, Action, Params = [H|_T], Pid) ->
  {token, Token} = H,
  case is_authorized(Token, Pid) of
    true -> gen_server:call(Pid, {Method, Action, Params});
    false -> {error, {not_allowed, 'Authorization required'}}
  end;
process_request(_Method, _Action, [], _Pid) ->
  {error, {not_allowed, 'Authorization required'}}.

kill_auth_worker(ServerPid, WorkerPid) ->
  gen_server:cast(ServerPid, {kill_auth_worker, WorkerPid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init({Sup}) ->
  {ok, #state{fsms = orddict:new(), tokens = orddict:new(), supervisor = Sup}}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%auth
handle_call({'GET', auth, [{appid, AppId}]}, _From, State) ->
  WorkerName = {task2_auth_worker, make_ref()},
  {ok, Pid} = supervisor:start_child(State#state.supervisor, ?AUTH_WORKER(WorkerName, self())),
  case task3_auth_worker:authorize(AppId, Pid) of
    {token, Token} ->
      NewFsm = orddict:store(Pid, Token, State#state.fsms),
      Tokens = orddict:store(Token, Pid, State#state.tokens),
      {reply, {result, Token}, State#state{fsms = NewFsm, tokens = Tokens}};
    {error, Error} ->
      {reply, {error, Error}, State}
  end;

%create account
handle_call({'POST', account, [{token, _Token}, {balance, Balance}]}, _From, State) ->
  case task3_model:add_account(Balance) of
    {error, Error} -> {reply, {error, Error}, State};
    AccountId -> {reply, {result, AccountId}, State}
  end;

%get list of accounts
handle_call({'GET', accounts, [{token, _Token}]}, _From, State) ->
  Accounts = task3_model:get_accounts(),
  {reply, {result, Accounts}, State};
%get list of transactions
handle_call({'GET', transactions, [{token, _Token},{aid, AccountId}]}, _From, State) ->
  case task3_model:get_transactions(AccountId) of
    {error, Error} -> {reply, {error, Error}, State};
    Transactions ->{reply, {result, Transactions}, State}
  end;
%delete account
handle_call({'DELETE', account, [{token, _Token},{aid, AccountId}]}, _From, State) ->
  case task3_model:delete_account(AccountId) of
    {error, Error} -> {reply, {error, Error}, State};
    _ -> {reply, {result, ok}, State}
  end;
%deposit amount
handle_call({'POST', amount, [{token, _Token},{aid, AccountId}, {amount, Amount}]}, _From, State) ->
  case  task3_model:add_transaction(AccountId, {1, Amount}) of
    {error, Error} -> {reply, {error, Error}, State};
    TransactionId -> {reply, {result, TransactionId}, State}
  end;
%withdraw amount
handle_call({'DELETE', amount, [{token, _Token},{aid, AccountId}, {amount, Amount}]}, _From, State) ->
  case  task3_model:add_transaction(AccountId, {-1, Amount}) of
    {error, Error} -> {reply, {error, Error}, State};
    TransactionId -> {reply, {result, TransactionId}, State}
  end;
%get current state of server
handle_call({state}, _From, State) ->
  {reply, State, State};
%catch API requests
handle_call(_Params, _From, State) ->
  {reply, {error, {not_found, method_not_exists}}, State}.

handle_cast({kill_auth_worker, Pid}, State) ->
  supervisor:terminate_child(State#state.supervisor, Pid),
  supervisor:delete_child(State#state.supervisor, Pid),
  Token = orddict:fetch(Pid, State#state.fsms),
  NewTokens = orddict:erase(Token, State#state.tokens),
  NewFsm = orddict:erase(Pid, State#state.fsms),
  {ok, State#state{fsms = NewFsm, tokens = NewTokens}};
handle_cast(_Params, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%%%===============================================
%%% Internal Functions
%%%===============================================
is_authorized(Token, ServerPid) ->
  State = get_state(ServerPid),
  orddict:is_key(binary_to_atom(Token, utf8), State#state.tokens).

get_state(ServerPid) ->
  gen_server:call(ServerPid, {state}).