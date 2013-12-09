-module(task3).
-include("../include/task3_database.hrl").

%% API
-export([
    start/0,
    stop/0
]).

-define(APPS, [cowlib, crypto, ranch, cowboy, mnesia, task3]).

%% ===================================================================
%% API functions
%% ===================================================================

start() ->
    ok = ensure_started(?APPS),
    ok = setup_tables(),
    ok = sync:go().

stop() ->
    sync:stop(),
    ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
    case application:start(App) of
        ok -> ensure_started(Apps);
        {error, {already_started, App}} -> ensure_started(Apps)
    end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
    application:stop(App),
    stop_apps(Apps).

setup_tables() ->
  R = mnesia:create_schema([node()]),
  io:format("Create schema: ~p~n", [R]),
  R1 = mnesia:create_table(accounts,  [
    {record_name, accounts},
    {attributes, record_info(fields, accounts)},
    {type, set}
  ]),

  io:format("Create table: ~p~n", [R1]),

  R2 = mnesia:create_table(account_transactions,  [
    {record_name, account_transactions},
    {attributes, record_info(fields, account_transactions)},
    {type, bag}
  ]),
  io:format("Create table: ~p~n", [R2]),
ok.