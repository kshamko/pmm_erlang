-module(task2).
-author("konstantin.shamko@gmail.com").

%% API
-export([calc/1]).

%%%===============================================
%%% API
%%%===============================================
calc(Filename) ->
  [BottomRow|[PreBottomRow|Triangle]] = lists:reverse(load_file(Filename)),
  calc(BottomRow, PreBottomRow, Triangle).

%%%===============================================
%%% Internal Functions
%%%===============================================

load_file(Filename) ->
  {ok, Device} = file:open(Filename, [read]),
  read_file(Device).

read_file(Device) ->
  case file:read_line(Device) of
    {ok, Data} -> [line_to_list(Data) | read_file(Device)];
    eof -> []
  end.

line_to_list(Line) ->
  lists:map(fun(X)-> {Int,_} = string:to_integer(X), Int end, string:tokens(string:strip(Line, right, $\n), " ")).

calc(Bottom, PreBottom, Triangle) when length(Triangle) == 0 ->
  Max = lists:max(Bottom),
  [TopVal|_] = PreBottom,
  Max + TopVal;
calc(Bottom, PreBottom, Triangle) ->
  [NewPreBottom|NewTriangle] = Triangle,
  calc(calc_bottom_row(PreBottom, Bottom, 1, []),NewPreBottom, NewTriangle).

calc_bottom_row(PreBottom, Bottom, Index, NewBottom) when Index =< length(PreBottom) ->
  E1 = lists:nth(Index, Bottom),
  E2 = lists:nth(Index+1, Bottom),
  CurInt = lists:nth(Index, PreBottom),
  if
    E1 > E2 -> NewItem =  CurInt + E1;
    true ->  NewItem =  CurInt + E2
  end,
  calc_bottom_row(PreBottom, Bottom, Index+1,lists:append(NewBottom, [NewItem]));
calc_bottom_row(_,_,_,NewBottom)->
  NewBottom.