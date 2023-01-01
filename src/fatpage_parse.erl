%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_parse).

-export(
   [parse/1]).

parse(String) when is_list(String) ->
    parse_abnf(String).

parse_abnf(String) ->
    Parsers = [fatpage_rfc5234, fatpage_bootstrap_abnf, abnfc_rfc4234],
    Constructors = [fatpage_construct, fatpage_bootstrap_construct],
    parse_construct(choose(Constructors), parse_abnf(choose(Parsers), String)).

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

parse_construct(CMod, Rules) ->
    erlang:display({cparser, CMod}),
    lists:map(mk_construct(CMod), Rules).

mk_construct(CMod) ->
    fun({rule, N, D, C}) ->
            case CMod:string(C) of
                {ok, Con, 0} -> {rule, N, D, Con};
                {ok, Con, L} -> error({constructor_error, C, Con, L});
                Err -> error(Err)
            end
    end.
