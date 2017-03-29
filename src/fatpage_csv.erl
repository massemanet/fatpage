%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(fatpage_csv).

-export([fold/3, parse/1]).

parse(Filename) ->
  fold(Filename, fun parser/2, []).

fold(Filename, Fun , Acc) ->
  fatpage:eof(fatpage_gen_csv:root(fatpage:init(Filename, Fun, Acc))).

parser([[]], Acc) -> Acc;
parser(eof, Acc) -> lists:reverse(Acc);
parser(Rec, Acc) -> [lists:reverse(Rec)|Acc].
