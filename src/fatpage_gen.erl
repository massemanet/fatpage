-module(fatpage_gen).

-export([parse/1,
         unroll/1,
         left_recursive/1,
         forms/1,
         pp/1]).

pp(Filename) ->
    lists:flatten([erl_pp:form(Form) || Form <- forms(Filename)]).

forms(Filename) ->
    gen_forms(unroll(Filename)).

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
    case is_all_final(Deriv) of
        true ->
            Rule;
        false ->
            {NewDerivs, NewRules} = finalize(Deriv),
            ?DERIV(Type, X, _) = Deriv,
            [?RULE(Name, Code, ?DERIV(Type, X, NewDerivs))|NewRules]
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
finalize(?DERIV(rep, _, Ds)) ->
    finalize(Ds, {[], []});
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code gen

gen_forms(Rules) ->
    {Forms, _} = lists:mapfoldl(fun template/2, 1, Rules),
    Forms.

template(?RULE(Name, Code, SubDeriv), Num) ->
    {template(Num, rule, Name, SubDeriv, Code), Num+1}.

template(Num, rule, Name, ?DERIV(Type, X, SubDeriv), _Code) ->
    {function, Num, to_atom(Name), 2,
     [{clause,Num,
       [{var,Num,'Ptr'}, {var,Num,'B'}],
       [],
       [template(Num, Type, X, SubDeriv)]}]}.

template(Num, char, X, D) ->
    template(Num, final, ?DERIV(char, X, D));
template(Num, rep, {Min, Max}, Deriv) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,repeat}},
     [template(Num, integer, Min),
      template(Num, integer, Max),
      template(Num, final, Deriv),
      {var,Num,'Ptr'},
      {var,Num,'B'}]};
template(Num, alt, {}, Derivs) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,alternative}},
     [template(Num, list, [template(Num, final, Deriv) || Deriv <- Derivs]),
      {var,Num,'Ptr'},
      {var,Num,'B'}]};
template(Num, seq, {}, Derivs) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,sequence}},
     [template(Num, list, [template(Num, final, Deriv) || Deriv <- Derivs]),
      {var,Num,'Ptr'},
      {var,Num,'B'}]}.

template(Num, final, ?DERIV(appl, _, Name)) ->
    {'fun',Num,{function,to_atom(Name),2}};
template(Num, final, ?DERIV(char, _, Cs)) when is_list(Cs) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,literal}},
     [template(Num, cguard, Cs),
      {var,Num,'Ptr'},
      {var,Num,'B'}]};
template(Num, final, ?DERIV(char, _, {C})) when is_integer(C) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,literal}},
     [template(Num, integer, C),
      {var,Num,'Ptr'},
      {var,Num,'B'}]};
template(Num, final, ?DERIV(char, _, {C1, C2})) ->
    {call,Num,
     {remote,Num,{atom,Num,fatpage},{atom,Num,literal}},
     [template(Num, integer, C1),
      template(Num, integer, C2),
      {var,Num,'Ptr'},
      {var,Num,'B'}]};
template(Num, cguard, Cs) ->
    {'fun',Num,
     {clauses,[{clause,Num,
                [{var,Num,'I'}],
                cguards(Num, Cs),
                [{atom,Num,true}]},
               {clause,Num,[{var,Num,'_'}],[],[{atom,Num,false}]}]}};
template(Num, integer, infinity) ->
    {atom, Num, infinity};
template(Num, integer, I) when is_integer(I) ->
    {integer, Num, I};
template(Num, list, L) ->
    lists:foldr(fun(E, O) -> {cons, Num, E, O} end, {nil, Num}, L).

cguards(Num, Cs) ->
    {Guards, _} = lists:mapfoldl(fun cguard/2, Num, Cs),
    Guards.

cguard({C}, Num) when is_integer(C) ->
    {op,Num,'==',{char,Num,C},{var,Num,'I'}};
cguard({C1, C2}, Num) when is_integer(C1), is_integer(C2) ->
    [{op,Num,'=<',{char,Num,C1},{var,Num,'I'}},
     {op,Num,'=<',{var,Num,'I'},{char,Num,C2}}].

to_atom(L) when is_list(L) -> list_to_atom(L);
to_atom(B) when is_binary(B) -> binary_to_atom(B);
to_atom(A) when is_atom(A) -> A.


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
