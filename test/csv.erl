%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(csv).

-export([go/1]).

-include("csv.abnf.hrl").

go(CSVfile) ->
    {ok, Bin} = file:read_file(CSVfile),
    {ok, L, []} = decode(binary_to_list(Bin)),
    [Tags, Vals|_] = L,
    maps:from_list(lists:zip(Tags, [reify(V)||V<-Vals])).

reify(V) ->
    try list_to_integer(string:trim(V, both))
    catch _:_ -> V
    end.
