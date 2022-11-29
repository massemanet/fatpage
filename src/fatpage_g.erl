%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_g).

-export(
   [preamble/2,
    rule/6]).

preamble(Mod, Entry) ->
    VF = stx('Filename'),
    VI = stx(implicit_fun, {Entry, 2}),
    [stx(attr, {module, [stx(Mod)]}),
     stx(attr, {export, [stx(fa, {file, 1}), stx(fa, {string, 1})]}),
     stx(func, {file, [VF], [], [stx(call, {fatpage, file, [VF, VI]})]}),
     stx(func, {string, [VF], [], [stx(call, {fatpage, string, [VF, VI]})]})].

rule(Line, Name, Code, Type, X, Sub) ->
    stx(set_pos, {rule(Name, Code, Type, X, Sub), Line}).

rule(Name, Code, Type, X, Sub) ->
    stx(func, {Name, ptr_b(), [], [rule_body(Code, Type, X, Sub)]}).

rule_body(nocode, Type, X, Sub) -> fcall(Type, X, Sub);
rule_body(Code, Type, X, Sub) -> fcase(Code, Type, X, Sub).

fcase(Code, Type, X, Deriv) ->
    Nbindings = nbindings(Type, Deriv),
    stx(case_, {fcall(Type, X, Deriv), [fclause({ok, Nbindings, Code}), fclause({err, 'Err'})]}).

-define(DERIV(Type, X, Ds), {deriv, Type, X, Ds}).
fcall(rep, {Min, Max}, Deriv) ->
    stx(call, {fatpage, repeat, [stx(Min), stx(Max), final(Deriv)|ptr_b()]});
fcall(alt, {}, Derivs) ->
    stx(call, {fatpage, alternative, [finals(Derivs)|ptr_b()]});
fcall(seq, {}, Derivs) ->
    stx(call, {fatpage, sequence, [finals(Derivs)|ptr_b()]});
fcall(final, char, Ds) ->
    stx(call, {fatpage, final, [final(?DERIV(final, char, Ds))|ptr_b()]});
fcall(final, appl, Ds) ->
    stx(call, {fatpage, final, [final(?DERIV(final, appl, Ds))|ptr_b()]}).

ptr_b() ->
    [stx('Ptr'), stx('B')].

fclause({ok, Nbindings, Code}) ->
    stx(clause, {[stx(tuple, [stx(ok), bindings(Nbindings, Code)])], [], Code});
fclause({err, V}) ->
    Var = stx(V),
    stx(clause, {[Var], [], [Var]}).

nbindings(rep, _) -> 1;
nbindings(alt, Derivs) -> length(Derivs);
nbindings(seq, Derivs) -> length(Derivs).

bindings(Nbindings, Code) ->
    BoundVars = bound_vars(Code),
    AvailVars = avail_vars(Nbindings),
    Wild = stx('Y'),
    case {BoundVars -- [Wild], BoundVars -- AvailVars} of
        {[], [Wild]} -> Wild;
        {BoundVars, []} -> match_vars(BoundVars, AvailVars);
        {BoundVars, [Wild]} -> stx(eq, {match_vars(BoundVars, AvailVars), Wild});
        Err -> error({unknown_vars, Err})
    end.

bound_vars(Code) ->
    lists:usort(bound_vars(Code, [])).

bound_vars({var, _, V}, Vs) ->
    [stx(V)|Vs];
bound_vars(L, Vs) when is_list(L) ->
    lists:foldl(fun bound_vars/2, Vs, L);
bound_vars(T, Vs) when is_tuple(T) ->
    bound_vars(tuple_to_list(T), Vs);
bound_vars(_, Vs) ->
    Vs.

avail_vars(N) ->
    [match_var(I) || I <- lists:seq(1, N)].

match_var(I) ->
    stx("Y"++integer_to_list(I)).

match_vars(BoundVars, AvailVars) ->
    stx(list, [match_var(V, BoundVars) || V <- AvailVars]).

match_var(V, Vs) ->    
    case lists:member(V, Vs) of
        true -> V;
        false -> stx('_')
    end.

finals(Derivs) ->
    stx(list, [final(Deriv) || Deriv <- Derivs]).

final(?DERIV(final, char, {C1, C2})) -> cfun([{C1, C2}]);
final(?DERIV(final, char, C0)) -> cfun([C0]);
final(?DERIV(final, appl, Name)) -> stx(implicit_fun, {Name, 2}).

cfun(Cs) ->
    stx(fun_,
        [{[stx('I')], cguard(Cs), [stx(true)]},
         {[stx('_')], [], [stx(false)]}]).

cguard(Cs) ->
    Var = stx('I'),
    [cop(Var, C) || C <- Cs].

cop(Var, {C1, C2}) ->
    [stx(infix, {stx(C1), '=<', Var}),
     stx(infix, {Var, '=<', stx(C2)})];
cop(Var, {C0}) when is_integer(C0)->
    [stx(infix, {stx(C0), '=:=', Var})].

%% Syntax manipulation

%% auto detect type (for var, int, and atom)
stx(A) when is_atom(A) ->
    stx(atom_to_list(A));
stx([I|_] = S) when $A =< I, I =< $Z; $_ == I ->
    stx(var, S);
stx([I|_] = S) when $ =< I, I =< $~ ->
    stx(atom, S);
stx(I) when is_integer(I) ->
    stx(integer, I).

%% primitives
stx(atom, A) -> erl_syntax:atom(A);
stx(integer, I) when is_integer(I) -> erl_syntax:integer(I);
stx(list, {H, T}) when is_list(H) -> erl_syntax:list(H, T);
stx(list, L) when is_list(L) -> stx(list, {L, none});
stx(tuple, L) when is_list(L) -> erl_syntax:tuple(L);

%% expressions
stx(call, {F, Args}) -> erl_syntax:application(stx(F), Args);
stx(call, {M, F, Args}) -> erl_syntax:application(erl_syntax:module_qualifier(stx(M), stx(F)), Args);
stx(case_, {Arg, Clauses}) -> erl_syntax:case_expr(Arg, Clauses);
stx(eq, {L, R}) -> erl_syntax:match_expr(L, R);
stx(fun_, Cs) -> erl_syntax:fun_expr(stx(clauses, Cs));
stx(infix, {Left, Op, Right}) -> erl_syntax:infix_expr(Left, stx(op, Op), Right);
stx(var, V) -> erl_syntax:variable(V);

%% syntax elements
stx(attr, {N, As}) -> erl_syntax:attribute(stx(N), As);
stx(clause, {Args, Guard, Body}) ->  erl_syntax:clause(Args, Guard, Body);
stx(clauses, Cs) -> [stx(clause, {Args, G, Body}) || {Args, G, Body} <- Cs];
stx(fa, {N, A}) -> erl_syntax:arity_qualifier(stx(N), stx(A));
stx(func, {Name, Clauses}) when is_list(Clauses)-> erl_syntax:function(stx(atom, Name), Clauses);
stx(func, {Name, Args, G, Body}) -> stx(func, {Name, stx(clauses, [{Args, G, Body}])});
stx(implicit_fun, {N, A}) -> erl_syntax:implicit_fun(stx(fa, {N, A}));
stx(op, Op) -> erl_syntax:operator(Op);

%% utilities
stx(set_pos, {Tree, Line}) -> erl_syntax:set_pos(Tree, Line).
