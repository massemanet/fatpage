%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_parse).

-export(
   [parse/1]).

parse(String) when is_list(String) ->
    parse_abnf(String).

parse_abnf(String) ->
    Parsers = [fatpage_rfc5234, fatpage_bootstrap_abnf, abnfc_rfc4234],
    Constructors = [fatpage_construct, fatpage_bootstrap_construct],
    postprocess(choose(Constructors), parse_abnf(choose(Parsers), String)).

choose(Ms) ->
    case lists:filter(fun is_available/1, Ms) of
        [] -> error({none_of, Ms});
        [M|_] -> M
    end.

is_available(Mod) ->
    [] =/= [M || {M, _, _} <- code:all_available(), M =:= atom_to_list(Mod)].

parse_abnf(Mod, String) ->
    erlang:display({parser, Mod}),
    {ok, Rs, 0} = Mod:string(String),
    Rs.

postprocess(CMod, Rules) ->
    erlang:display({cparser, CMod}),
    lists:map(postp(CMod), Rules).

%% drop lines that are not rules (e.g. empty lines)
%% run the construct parser if there is code
postp(CMod) ->
    fun({rule, [N, O, D, C]}) ->
            case C of
                {construct, Str} ->
                    case CMod:string(Str) of
                        {ok, Tree, 0} -> {rule, N, O, D, Tree};
                        {ok, Tree, L} -> error({constructor_error, Str, Tree, L});
                        Err -> error(Err)
                    end;
                _ ->
                    {rule, N, O, D, undefined}
            end;
       ({comment, C}) ->
            {comment, C};
       (WS) ->
            {whitespace, WS}
    end.
