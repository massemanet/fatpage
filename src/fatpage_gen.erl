%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(fatpage_gen).

-export([parse/1]).

parse(Filename) ->
  {ok, B} = file:read_file(Filename),
  {ok, {rulelist, _, Rules}, []} = abnfc:parse(binary_to_list(B)),
  lists:map(fun rules/1, Rules).

rules({rule, def_rule, RuleName, Prod, Code}) ->
  {RuleName, Prod, Code}.
