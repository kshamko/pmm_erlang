%%%-------------------------------------------------------------------
%%% @author kostik
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2013 2:12 PM
%%%-------------------------------------------------------------------
-module(task3_client_index_controller, [Req]).
-compile(export_all).

index('GET', []) ->
  Token = do_api_auth(),
  {array, Accounts} = do_api_call(get, [{do, accounts}, {token, Token}]),
  {ok, [{accounts, Accounts}]}.

 add_account('GET', []) ->
  {ok, []};
add_account('POST',[]) ->
  Token = do_api_auth(),
  Response = do_api_call(post, [{do, account}, {balance, Req:post_param("balance")}, {token, Token}]),
  io:format("P ~p~n", [Response]),
  {redirect,"/index/index"}.

transactions('GET', ["aid", AccountId]) ->
  Token = do_api_auth(),
  {array, Transactions} = do_api_call(get, [{do, transactions},{aid, list_to_atom(AccountId)}, {token, Token}]),
  {ok, [{transactions, Transactions}, {account_id, AccountId}]}.

add_transaction('GET', ["aid", AccountId]) ->
  {ok,[{account_id, AccountId}]};
add_transaction('POST', []) ->
  Token = do_api_auth(),
  Amount = Req:post_param("amount"),
  AccId = Req:post_param("aid"),
  do_api_call(post, [{do, amount}, {amount, Amount}, {aid, AccId}, {token, Token}]),
  {redirect,"/index/transactions/aid/"++AccId}.

del_transaction('GET', ["aid", AccountId]) ->
  {ok,[{account_id, AccountId}]};
del_transaction('POST', []) ->
  Token = do_api_auth(),
  Amount = Req:post_param("amount"),
  AccId = Req:post_param("aid"),
  do_api_call(delete, [{do, amount}, {amount, list_to_atom(Amount)}, {aid, list_to_atom(AccId)}, {token, Token}]),
  {redirect,"/index/transactions/aid/"++AccId}.

del_account('GET', ["aid", AccountId]) ->
  Token = do_api_auth(),
  do_api_call(delete, [{do, account}, {aid, list_to_atom(AccountId)}, {token, Token}]),
  {redirect,"/"}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% TODO get rid of jiffy here
do_api_call(Method, Params) when (Method =:= get) orelse (Method =:= delete)->
  Url = string:concat("http://localhost:8008/?", prepare_get_params(Params)),
  send_request(Method, {Url, []});
do_api_call(Method, Params) ->
  Url = "http://localhost:8008",
  send_request(Method, {Url, [], "application/json", jiffy:encode({Params})}).

do_api_auth() ->
  list_to_atom(do_api_call(get, [{do, auth}, {appid, '1234567'}])).

send_request(Method, RequestBody)->
  HTTPOptions = [],
  Options = [],
  {ok, {{"HTTP/1.1", _ReturnCode, _State}, _Head, Body}} = httpc:request(Method, RequestBody, HTTPOptions, Options),
  io:format("Body ~p~n", [Body]),
  case destruct(mochijson:decode(Body)) of
    [{"result", Result}] -> Result;
    [{"error", _Error}] -> []
  end.

prepare_get_params(Params) ->
  string:join(lists:map(fun({Key,Value}) -> string:join([atom_to_list(Key),atom_to_list(Value)],"=") end, Params),"&").

%% @doc Flatten {struct, [term()]} to [term()] recursively.
destruct({struct, L}) ->
  destruct(L);
destruct([H | T]) ->
  [destruct(H) | destruct(T)];
destruct({K, V}) ->
  {K, destruct(V)};
destruct(Term) ->
  Term.