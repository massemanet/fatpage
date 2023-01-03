%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_refine).

-export(
   [r2r/1,
    unroll/1,
    left_recursive/1]).

r2r(Rules) ->
    lists:map(fun rec/1, lists:filter(fun drop/1, Rules)).

unroll(Rules) ->
    erase(fatpage),
    unroll(r2r(Rules), []).

left_recursive(Rules) ->
    fixpoint({Rules, predefined()}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% drop everythigng that's not a rule

drop(X) ->
    element(1, X) =:= rule.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(deriv, {type, x = {}, ds}).
-record(rule, {name, deriv, code = <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% rewrite bootstrap parser output to internal form

-define(IS_CH(C), is_integer(C)).
%% a rule
rec({rule, N, '=', D, C}) -> #rule{name = N, code = C, deriv = rec(D)};

%% derivs
rec(D) when is_record(D, deriv) -> D;
rec(Ds) when is_list(Ds) -> lists:map(fun rec/1, Ds);
rec({seq, [Seq]})        -> rec(Seq);
rec({seq, Seqs})         -> #deriv{type = seq, ds = rec(Seqs)};
rec({alt, [Alt]})        -> rec(Alt);
rec({alt, Alts})         -> #deriv{type = alt, ds = rec(Alts)};
rec({rep, [], D})        -> rec(D);
rec({rep, [{1, 1}], D})  -> rec(D);
rec({rep, [Rep], D})     -> #deriv{type = rep, x = Rep, ds = rec(D)};
rec({app, App})          -> #deriv{type = final, x = appl, ds = App};
rec({C1, {dash, C2}})    -> #deriv{type = final, x = char, ds = {range, C1, C2}};
rec({char, C})           -> #deriv{type = final, x = char, ds = {char, C}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    Deriv = Rule#rule.deriv,
    case is_all_final(Deriv) of
        true ->
            Rule;
        false ->
            {NewDerivs, NewRules} = finalize(Deriv),
            [Rule#rule{deriv = Deriv#deriv{ds = NewDerivs}}|NewRules]
    end.

is_all_final(#deriv{type = Type, ds = Ds}) ->
    case Type of
        final -> true;
        rep -> is_final(Ds);
        alt -> lists:all(fun is_final/1, Ds);
        seq -> lists:all(fun is_final/1, Ds)
    end.

is_final(#deriv{type = Type}) -> is_final(Type);
is_final(final) -> true;
is_final(_) -> false.

finalize(#deriv{type = Type, ds = Ds}) when Type == seq; Type == alt ->
    lists:foldl(fun finalize/2, {[], []}, Ds);
finalize(#deriv{type = rep, ds = Ds}) ->
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
    append_thing(#deriv{type = final, x = appl, ds = Name}, Derivs).

append_rule(Name, Deriv, Rules) ->
    append_thing(#rule{name = Name, deriv = Deriv}, Rules).

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
    list_to_atom(flat("-virtual-~p-", [I])).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% detect left-recursion
%% we go through the list of rules and rewrite them (to
%% 'nullable'/'non_nullable') until fixpoint. If the resulting list of
%% rules is non-empty, we have left-recursion.
fixpoint({Rules, Final}) ->
    case fix(Rules, Final, []) of
        {Rules, Final} -> {Rules, Final};
        RF -> fixpoint(RF)
    end.

predefined() ->
    [{'ALPHA', non_nullable},
     {'BIT', non_nullable},
     {'CR', non_nullable},
     {'CR', non_nullable},
     {'CRLF', non_nullable},
     {'DIGIT', non_nullable},
     {'DQUOTE', non_nullable},
     {'EOF', non_nullable},
     {'HEXDIG', non_nullable},
     {'HTAB', non_nullable},
     {'LF', non_nullable},
     {'LF', non_nullable},
     {'SP', non_nullable},
     {'VCHAR', non_nullable},
     {'WS', non_nullable},
     {'WSP', non_nullable}].


fix([], Final, ORs)                              -> {lists:usort(ORs), lists:usort(Final)};
fix([#rule{name = N, deriv = D}|Rs], Final, ORs) -> fix([{N, D}|Rs], Final, ORs);
fix([{Name, V}|Rs], Final, ORs) when is_atom(V)  -> fix(Rs, [{Name, V}|Final], ORs);
fix([{Name, Deriv}|Rs], Final, ORs)              -> fix(Rs, Final, [{Name, type(Deriv, Final)}|ORs]).

type(non_nullable, _)                               -> non_nullable;
type(nullable, _)                                   -> nullable;
type(#deriv{type = rep, x = {0, _}}, _)             -> nullable;
type(#deriv{type = rep, ds = D}, _)                 -> D;
type(#deriv{type = final, x = char}, _)             -> non_nullable;
type(#deriv{type = final, x = appl, ds = 'EOF'}, _) -> non_nullable;
type(#deriv{type = final, x = appl, ds = N}, Final) -> rewrite_name(N, Final);
type(#deriv{type = alt, ds = As}, Final)            -> rewrite_alt(As, Final);
type(#deriv{type = seq, ds = Ss}, Final)            -> rewrite_seq(Ss, Final).

rewrite_name(Name, Final) ->
    case proplists:get_value(Name, Final, no) of
        no -> #deriv{type = final, x = appl, ds = Name};
        V -> V
    end.

rewrite_alt(As, Final) ->
    case is_nullable(As) of
        no      -> non_nullable;
        yes     -> nullable;
        perhaps -> #deriv{type = alt, ds = recurse(As, Final)}
    end.

rewrite_seq([], _) -> nullable;
rewrite_seq(Ss, Final) ->
    case hd(Ss) of
        non_nullable -> non_nullable;
        nullable     -> #deriv{type = seq, ds = tl(Ss)};
        _            -> #deriv{type = seq, ds = recurse(Ss, Final)}
    end.

recurse(Ds, Final) ->
    [type(D, Final) || D <- Ds].

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
