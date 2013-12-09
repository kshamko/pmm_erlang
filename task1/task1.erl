%%%-------------------------------------------------------------------
%%% @author kostik
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2013 8:58 PM
%%%-------------------------------------------------------------------
-module(task1).
-author("kostik").

%% API
-export([get/1]).

get(List) ->
  Sum = sum(List, 0),
  [H|_] = List,
  get(List, 0, Sum, [abs(Sum-H)|H]).


get([Element|_], SumLeft, SumRight, _) when SumLeft == (SumRight - Element) ->
  Element;
get([Element|T], SumLeft, SumRight, [Diff|El]) ->
  NewDiff = abs(SumRight - SumLeft - Element),
  if NewDiff < Diff
    -> Pos = [NewDiff|Element];
    true -> Pos = [Diff|El]
  end,
  get(T, SumLeft + Element, SumRight - Element, Pos);
get(_, _, _, [_|Element]) ->
  Element.


sum([H|T], Sum) ->
  sum(T, Sum + H);
sum(_, Sum) ->
  Sum.