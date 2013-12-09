%%%-------------------------------------------------------------------
%%% @author kostik
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Nov 2013 4:19 PM
%%%-------------------------------------------------------------------
-module(task3_model).
-author("kostik").
-include("../include/task3_database.hrl").

%% API
-export([add_account/1, get_accounts/0, get_transactions/1, add_transaction/2, delete_account/1]).

add_account(Balance) when is_binary(Balance) ->
  Amount = bin_to_num(Balance),
  case Amount of
    false -> {error, {input, 'Bad money amount format'}};
    Amount ->
      AccountId = unique_id(),
      Fun = fun() ->
        mnesia:write(#accounts{balance = Amount, account_id = AccountId}),
        mnesia:write(#account_transactions{account_id = AccountId, amount = Amount, transaction_id = unique_id()})
      end,

      case mnesia:transaction(Fun) of
        {atomic, ok} -> AccountId;
        _ -> {error, {transaction, 'Unknown error'}}
      end
  end;
add_account(_Balance) ->
  {error, {input, 'Bad money amount format'}}.

get_accounts() ->
  prepare_records(mnesia:dirty_select(accounts, [{'_', [], ['$_']}]), []).

%%
%%
%%
add_transaction(AccountId, {Mult, Amount}) when is_binary(Amount) ->
  NumAmount = bin_to_num(Amount),
  Account = mnesia:dirty_read({accounts, AccountId}),

  case {NumAmount, Account} of
    {false, _} -> {error, {input, 'Bad money amount format'}};
    {_, []} -> {error, {not_found, 'Bad account id'}};
    _ ->
      TransactionId = unique_id(),
      Fun = fun() ->
        [P] = mnesia:wread({accounts, AccountId}),
        case [P] of
          [{accounts, _Id, CurrentAmount}] ->
            NewAmount = CurrentAmount + Mult * NumAmount,
            mnesia:write(#account_transactions{account_id = AccountId, amount = Mult * NumAmount, transaction_id = TransactionId}),
            mnesia:write(P#accounts{balance = NewAmount});
          [] -> {error}
        end
      end,
      case mnesia:transaction(Fun) of
        {atomic, ok} -> TransactionId;
        _ -> {error, {transaction, 'Unknown error'}}
      end
  end;
add_transaction(_AccountId, _Amount) ->
  {error, {input, 'Bad money amount format'}}.

get_transactions(AccountId) ->
  case mnesia:dirty_read({accounts, AccountId}) of
    [] -> {error, {not_found, 'Bad account id'}};
    _ ->
      prepare_records(mnesia:dirty_select(account_transactions, [{#account_transactions{account_id = '$1', _ = '_'}, [{'==', '$1', AccountId}], ['$_']}]), [])
  end.

delete_account(AccountId) ->
  case mnesia:dirty_read({accounts, AccountId}) of
    [] -> {error, {not_found, 'Bad account id'}};
    _ ->
      Fun = fun() ->
        List = mnesia:match_object(#account_transactions{account_id = AccountId, _ = '_'}),
        lists:foreach(fun(X) -> mnesia:delete_object(X) end, List),
        mnesia:delete({accounts, AccountId})
      end,
      mnesia:transaction(Fun)
  end.


%%%========================================
%%% Internal Functions
%%%========================================
prepare_records([{accounts, Id, Balance}|Raw], Result) ->
  prepare_records(Raw, [[{balance, Balance}, {account_id, Id}]|Result]);
prepare_records([{account_transactions, Id, Amount, AccountId}|Raw], Result) ->
  prepare_records(Raw, [[{account_id, AccountId}, {amount, Amount}, {transaction_id, Id}]|Result]);
prepare_records([], Result) ->
  Result.

%%
unique_id() ->
  {Mega, Secs, Micro} = erlang:now(),
  Timestamp = Mega * 1000 * 1000 * 1000 * 1000 + Secs * 1000 * 1000 + Micro,
  tidy_hash(crypto:hash(md5, integer_to_binary(Timestamp))).

%%
bin_to_num(Bin) ->
  N = binary_to_list(Bin),
  case string:to_float(N) of
    {error, no_float} ->
      try list_to_integer(N)
      catch
        error:badarg -> false
      end;
    {F, _Rest} -> F
  end.

%% TODO move to hrl
tidy_hash(Hash) when is_binary(Hash) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash])).