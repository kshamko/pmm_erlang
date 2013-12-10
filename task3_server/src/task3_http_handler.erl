-module(task3_http_handler).

-export([init/3, rest_init/2]).
-export([
  content_types_provided/2,
  content_types_accepted/2,
  allowed_methods/2]).
-export([process_body_request/2, process_request/2, delete_resource/2, delete_completed/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, [{auth_server_pid, Pid}|_]) ->
  {ok, Req, {auth_server_pid, Pid}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"DELETE">>, <<"POST">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, process_request}], Req, State}.

%to process POSTs
content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, process_body_request}], Req, State}.

%to process DELETEs
delete_resource(Req, State) ->
  {true, Req, State}.

delete_completed(Req, State) ->
  process_body_request(Req, State).


process_request(Req, State = {auth_server_pid, Pid}) ->
  {Method, Req2} = cowboy_req:method(Req),
  case cowboy_req:has_body(Req) of
    true ->
      {ok, Json, _Req} = cowboy_req:body(Req),
      Qs = destruct(mochijson2:decode(Json));
    false -> {Qs, _Req3} = cowboy_req:qs_vals(Req2)
  end,

  {Action, Params} = prepare_request_params(Qs, {empty_action, []}),
  io:format("Method: ~p, Action: ~p, Params:~p ~n", [Method, Action, Params]),

  case Response = task3_server:process_request(binary_to_atom(Method, utf8), Action, Params, Pid) of
    {error, {_ErrorType, Error}} ->
      {ok, Req3} = cowboy_req:reply(http_status(Response), [{<<"content-type">>, <<"application/json">>}], mochijson2:encode({[{error, Error}]}), Req),
      {halt, Req3, State};
    {result, Result} ->
      {ok, Req3} = cowboy_req:reply(http_status(Method), [{<<"content-type">>, <<"application/json">>}], mochijson2:encode({[{result, Result}]}), Req),
      {halt, Req3, State}
  end.

process_body_request(Req, State) ->
  process_request(Req, State).

%%%===============================================
%%% Internal Functions
%%%===============================================
prepare_request_params([{Name, Value}|Params], {Action, Prepared}) when Name =/= <<"do">>, is_binary(Value) ->
  prepare_request_params(Params, {Action, [{binary_to_atom(Name, utf8), Value}|Prepared]});
prepare_request_params([{Name, Value}|Params], {Action, Prepared}) when Name =/= <<"do">>, is_list(Value) ->
  prepare_request_params(Params, {Action, [{binary_to_atom(Name, utf8), list_to_binary(Value)}|Prepared]});
prepare_request_params([{Name, Value}|Params], {Action, Prepared}) when Name =/= <<"do">>, is_atom(Value) ->
  prepare_request_params(Params, {Action, [{binary_to_atom(Name, utf8), atom_to_binary(Value, utf8)}|Prepared]});
prepare_request_params([{Name, Value}|Params], {Action, Prepared}) when Name =/= <<"do">> ->
  prepare_request_params(Params, {Action, [{binary_to_atom(Name, utf8), Value}|Prepared]});
prepare_request_params([{Name, Value}|Params], {_Action, Prepared}) when Name =:= <<"do">> ->
  prepare_request_params(Params, {binary_to_atom(Value, utf8), Prepared});
prepare_request_params([], Prepared) ->
  Prepared.


http_status({error, {ErrorType, _Error}}) ->
  case ErrorType of
    not_found -> 404;
    not_allowed -> 403;
    _ -> 500
  end;
http_status(_) ->
  200.

%% @doc Flatten {struct, [term()]} to [term()] recursively.
destruct({struct, L}) ->
  destruct(L);
destruct([H | T]) ->
  [destruct(H) | destruct(T)];
destruct({K, V}) ->
  {K, destruct(V)};
destruct(Term) ->
  Term.