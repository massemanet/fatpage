-module(fatpage_gen).

-export([parse/1,
         unroll/1,
         left_recursive/1]).

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
-define(DERIV_TYPE(T), ?DERIV(T, _, _)).
-define(DERIV_DERIVS(Ds), ?DERIV(_, _, Ds)).
-define(RULE(Name, Code, Deriv), {rule, Name, Deriv, Code}).
-define(RULE_NAME(N), ?RULE(N, _, _)).
-define(RULE_DERIV(D), ?RULE(_, D, _)).

reify({rule, def_rule, Name, Deriv, Code}) -> ?RULE(Name, Code, reify(Deriv));
reify(?DERIV(_, _, _) = D) -> D;
reify({rulename, Name})           -> ?DERIV(appl, {}, Name);
reify({alt, Alts})                -> ?DERIV(alt, {}, lists:map(fun reify/1, Alts));
reify({char_alt, Alts})           -> ?DERIV(alt, {}, lists:map(fun reify/1, Alts));
reify({seq, Seqs})                -> ?DERIV(seq, {}, lists:map(fun reify/1, Seqs));
reify({char_seq, Seqs})           -> ?DERIV(seq, {}, lists:map(fun reify/1, Seqs));
reify({repeat, Min, Max, Rep})    -> ?DERIV(rep, {Min, Max}, reify(Rep));
reify({char_val, Char})           -> ?DERIV(char, {}, {Char});
reify({char_range, Char1, Char2}) -> ?DERIV(char, {}, {Char1, Char2}).

%% unroll nested rules
unroll([], Orules) ->
    lists:reverse(Orules);
unroll([Rule|Rules], Orules) ->
    case expand_rule(Rule) of
        Rule -> unroll(Rules, [Rule|Orules]);
        NewRules -> unroll(NewRules++Rules, Orules)
    end.

expand_rule(Rule) ->
    ?RULE(Name, Code, Deriv) = Rule,
    ?DERIV(Type, _, _) = Deriv,
    case is_all_final(Deriv) of
        true -> Rule;
        false ->
            {NewDerivs, NewRules} = finalize(Deriv),
            [?RULE(Name, Code, ?DERIV(Type, {}, NewDerivs))|NewRules]
    end.

is_all_final(Derivs) when is_list(Derivs) ->
    lists:all(fun is_all_final/1, Derivs);
is_all_final(?DERIV(Type, _, Ds)) ->
    case Type of
        appl -> true;
        char -> true;
        rep -> is_final(Ds);
        alt -> lists:all(fun is_final/1, Ds);
        seq -> lists:all(fun is_final/1, Ds)
    end.

is_final(?DERIV(T, _, _)) -> is_final(T);
is_final(T) -> not lists:member(T, [alt, seq, rep]).

finalize(?DERIV(Type, _, Ds)) when Type == seq; Type == alt ->
    lists:foldl(fun finalize/2, {[], []}, Ds);
finalize(?DERIV_TYPE(rep) = ?DERIV_DERIVS(Ds)) ->
    finalize(Ds, {[], []});
finalize(Deriv) ->
    {Deriv, []}.

finalize(Deriv, {Derivs, Rules}) ->
    case is_final(Deriv) of
        true ->
            {append_thing(Deriv, Derivs), Rules};
        false ->
            case get_name(Deriv) of
                {new, Name} ->
                    {append_deriv(appl, Name, Derivs), append_rule(Name, Deriv, Rules)};
                {old, Name} ->
                    {append_deriv(appl, Name, Derivs), []}
            end
    end.

append_deriv(appl, Name, Derivs) ->
    append_thing(?DERIV(appl, {}, Name), Derivs).

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
