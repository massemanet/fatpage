-module(fatpage_gen).

-export([parse/1,
         unroll/1,
         left_recursive/1,
         forms/1,
         pp/1,
         display/1]).

display(Filename) ->
    io:fwrite("~s~n", [pp(Filename)]).

pp(Filename) ->
    lists:flatmap(fun(F) -> try erl_prettypr:format(F)++[10] catch C:R -> erlang:display({C, R, F}),"\n :( \n" end end, forms(Filename)).

forms(Filename) ->
    gen_forms(mod(Filename), unroll(Filename)).

left_recursive(Filename) ->
    fixpoint({unroll(Filename), []}).

unroll(Filename) ->
    erase(fatpage),
    unroll(parse(Filename), []).

parse(Filename) ->
    {ok, B} = file:read_file(Filename),
    {rulelist, _, Rules} = abnfc:parse(binary_to_list(B), []),
    lists:map(fun reify/1, Rules).

-define(DERIV(Type, X, Ds), {deriv, Type, X, Ds}).
-define(RULE(Name, Code, Deriv), {rule, Name, Deriv, Code}).

%% rewrite bootstrap parser output to internal form
reify({rule, def_rule, Name, Deriv, Code}) -> ?RULE(Name, Code, reify(Deriv));
reify(?DERIV(_, _, _) = D) -> D;
reify({rulename, Name})           -> ?DERIV(final, appl, Name);
reify({alt, Alts})                -> ?DERIV(alt, {}, lists:map(fun reify/1, Alts));
reify({char_alt, Alts})           -> ?DERIV(alt, {}, lists:map(fun reify/1, Alts));
reify({seq, Seqs})                -> ?DERIV(seq, {}, lists:map(fun reify/1, Seqs));
reify({char_seq, Seqs})           -> ?DERIV(seq, {}, lists:map(fun reify/1, Seqs));
reify({repeat, Min, Max, Rep})    -> ?DERIV(rep, {Min, Max}, reify(Rep));
reify({char_val, Char})           -> ?DERIV(final, char, {Char});
reify({char_range, Char1, Char2}) -> ?DERIV(final, char, {Char1, Char2}).

%% unroll nested rules
unroll([], Orules) ->
    lists:reverse(Orules);
unroll([Rule|Rules], Orules) ->
    try expand_rule(Rule) of
        Rule -> unroll(Rules, [Rule|Orules]);
        NewRules -> unroll(NewRules++Rules, Orules)
    catch C:R:S -> error({C, R, Rule, S})
    end.

expand_rule(Rule) ->
    ?RULE(Name, Code, Deriv) = Rule,
    case is_all_final(Deriv) of
        true ->
            Rule;
        false ->
            {NewDerivs, NewRules} = finalize(Deriv),
            ?DERIV(Type, X, _) = Deriv,
            [?RULE(Name, Code, ?DERIV(Type, X, NewDerivs))|NewRules]
    end.

is_all_final(?DERIV(Type, _, Ds)) ->
    case Type of
        final -> true;
        rep -> is_final(Ds);
        alt -> lists:all(fun is_final/1, Ds);
        seq -> lists:all(fun is_final/1, Ds)
    end.

is_final(?DERIV(T, _, _)) -> is_final(T);
is_final(T) when is_atom(T) -> final == T.

finalize(?DERIV(Type, _, Ds)) when Type == seq; Type == alt ->
    lists:foldl(fun finalize/2, {[], []}, Ds);
finalize(?DERIV(rep, _, Ds)) ->
    {[D], Rs} = finalize(Ds, {[], []}),
    {D, Rs};
finalize(Deriv) ->
    {Deriv, []}.

finalize(Deriv, {Derivs, Rules}) ->
    case is_final(Deriv) of
        true ->
            {append_thing(Deriv, Derivs), Rules};
        false ->
            rewrite_deriv(Deriv, Derivs, Rules)
    end.

rewrite_deriv(Deriv, Derivs, Rules) ->
    case get_name(Deriv) of
        {new, Name} ->
            {append_deriv(appl, Name, Derivs), append_rule(Name, Deriv, Rules)};
        {old, Name} ->
            {append_deriv(appl, Name, Derivs), Rules}
    end.

append_deriv(appl, Name, Derivs) ->
    append_thing(?DERIV(final, appl, Name), Derivs).

append_rule(Name, Deriv, Rules) ->
    append_thing(?RULE(Name, nocode, Deriv), Rules).

append_thing(T, Ts) ->
    Ts++[T].

get_name(Deriv) ->
    case get(fatpage) of
        undefined ->
            put(fatpage, #{}),
            get_name(Deriv);
        #{{deriv, Deriv} := Id} ->
            {old, make_name(Id)};
        #{id := Id} = Fatpage ->
            put(fatpage, Fatpage#{id => Id+1, {deriv, Deriv} => Id}),
            {new, make_name(Id)};
        Fatpage ->
            put(fatpage, Fatpage#{id => 1, {deriv, Deriv} => 0}),
            {new, make_name(0)}
    end.

make_name(I) ->
    lists:flatten(io_lib:format("-virtual-~p-", [I])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code gen

mod(Filename) ->
    filename:basename(Filename, ".abnf").

gen_forms(Mod, Rules) ->
    {Forms, _} = lists:mapfoldl(fun template/2, 1, Rules),
    FirstRule = erl_syntax:atom_value(erl_syntax:function_name(hd(Forms))),
    fatpage_g:preamble(Mod, FirstRule)++Forms.

template(?RULE(Name, Code, ?DERIV(Type, X, SubDeriv)), Num) ->
    {fatpage_g:rule(Num, Name, Code, Type, X, SubDeriv), Num+1}.

%% we go through the list of rules and rewrite them (to
%% 'nullable'/'non_nullable') until fixpoint. If the resulting list of
%% rules is non-empty, we have left-recursion.
fixpoint({Rules, Final}) ->
    case fix(Rules, Final, []) of
        {Rules, Final} -> Rules;
        RF -> fixpoint(RF)
    end.

fix([], Final, ORs)                                   -> {lists:usort(ORs), lists:usort(Final)};
fix([{rule, def_rule, Name, Rule, _}|Rs], Final, ORs) -> fix([{Name, Rule}|Rs], Final, ORs);
fix([{Name, V}|Rs], Final, ORs) when is_atom(V)       -> fix(Rs, [{Name, V}|Final], ORs);
fix([{Name, Rule}|Rs], Final, ORs)                    -> fix(Rs, Final, [{Name, type(Rule, Final)}|ORs]).

type(non_nullable, _)          -> non_nullable;
type(nullable, _)              -> nullable;
type({char_val, _}, _)         -> non_nullable;
type({char_range, _, _}, _)    -> non_nullable;
type({{repeat, 0, _}, _}, _)   -> nullable;
type({{repeat, _, _}, [R]}, _) -> R;
type({rulename, R}, Final)     -> rewrite_rulename(R, Final);
type({alt, As}, Final)         -> rewrite_alt(As, Final);
type({seq, Ss}, Final)         -> rewrite_seq(Ss, Final).

rewrite_rulename(R, Final) ->
    case proplists:get_value(R, Final, no) of
        no -> {rulename, R};
        V -> V
    end.

rewrite_alt(As, Final) ->
    case is_nullable(As) of
        no -> non_nullable;
        yes -> nullable;
        perhaps -> {alt, recurse(As, Final)}
    end.

rewrite_seq([], _) -> nullable;
rewrite_seq(Ss, Final) ->
    case hd(Ss) of
        non_nullable -> non_nullable;
        nullable -> {seq, tl(Ss)};
        _ -> {seq, recurse(Ss, Final)}
    end.

recurse(Rs, Final) ->
    [type(R, Final) || R <- Rs].

is_nullable(As) ->
    case {all_non_nullable(As), any_nullable(As)} of
        {true, false} -> no;
        {false, true} -> yes;
        {false,false} -> perhaps
    end.

all_non_nullable(L) ->
    lists:all(fun(V) -> V == non_nullable end, L).

any_nullable(L) ->
    lists:any(fun(V) -> V == nullable end, L).
