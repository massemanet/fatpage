%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_g).

-export(
   [fold_forms/3,
    forms/2]).


-record(rule, {name, deriv, code}).
-record(deriv, {type, x, ds}).

fold_forms(F, Acc0, Forms) ->
    lists:flatmap(fun(Form) -> fold_form(F, Acc0, Form) end, Forms).

fold_form(F, Acc0, Form) ->
    stx(rev, stx(fold, {F, Acc0, Form})).

forms(Mod, Rules) ->
    {Forms, _} = lists:mapfoldl(fun rule/2, 1, Rules),
    #rule{name=FirstRule} = hd(Rules),
    Header = header(Mod, FirstRule),
    stx(revert, Header++Forms).

rule(#rule{name=Name, code=Code, deriv=#deriv{type=Type, x=X, ds=SubDeriv}}, Num) ->
    {rule(Num, Name, Code, Type, X, SubDeriv), Num+1}.

header(Mod, Entry) ->
    IF = stx(rule_fun, Entry),
    FN = stx('Filename'),
    [stx(attr, {module, [stx(Mod)]}),
     stx(attr, {export, [stx(list, [stx(fa, {file, 1}), stx(fa, {string, 1})])]}),
     stx(func, {file, [FN], [], [stx(call, {file, [FN, IF]})]}),
     stx(func, {string, [FN], [], [stx(call, {string, [FN, IF]})]})].

rule(Line, Name, Code, Type, X, Sub) ->
    stx(set_pos, {rule(Name, Code, Type, X, Sub), Line}).

rule(Name, Code, Type, X, Sub) ->
    stx(func, {stx(rule, Name), [stx('Obj')], [], [rule_body(Code, Type, X, Sub)]}).

rule_body(nocode, Type, X, Sub) -> fcall(Type, X, Sub);
rule_body(Code, Type, X, Sub) -> fcase(Code, Type, X, Sub).

fcase(Code, Type, X, Deriv) ->
    Nbindings = nbindings(Type, Deriv),
    stx(case_, {fcall(Type, X, Deriv), [fclause({ok, Nbindings, Code}), fclause({err, 'Err'})]}).

-define(DERIV(Type, X, Ds), {deriv, Type, X, Ds}).
fcall(rep, {Min, Max}, Deriv) ->
    stx(call, {repeat, [stx(Min), stx(Max), final(Deriv), stx('Obj')]});
fcall(alt, {}, Derivs) ->
    stx(call, {alternative, [finals(Derivs), stx('Obj')]});
fcall(seq, {}, Derivs) ->
    stx(call, {sequence, [finals(Derivs), stx('Obj')]});
fcall(final, char, Ds) ->
    stx(call, {final, [final(#deriv{type=final, x=char, ds=Ds}), stx('Obj')]});
fcall(final, appl, Ds) ->
    stx(call, {final, [final(#deriv{type=final, x=appl, ds=Ds}), stx('Obj')]}).

fclause({ok, Nbindings, [Code]}) ->
    Match = stx(tuple, [stx(ok), bindings(Nbindings, Code), stx('O')]),
    Ret = stx(tuple, [stx(ok), Code, stx('O')]),
%    Ret = {tree,tuple,
%           {attr,0,[],none},
%           [{tree,atom,{attr,0,[],none},ok},
%            {tree,variable,{attr,0,[],none},'Y1'},
%            {tree,variable,{attr,0,[],none},'O'}]},
%    Ret = {tuple,0,[{atom,0,ok},Code,{var,0,'O'}]},
    stx(clause, {[Match], [], [Ret]});
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

final(#deriv{type=final, x=char, ds={C1, C2}}) -> cfun([{C1, C2}]);
final(#deriv{type=final, x=char, ds=C0}) -> cfun([C0]);
final(#deriv{type=final, x=appl, ds=Name}) -> stx(rule_fun, Name).

cfun(Cs) ->
    stx(fun_,
        [{[stx(bin, ['I'])], cguard(Cs), [stx(true)]},
         {[stx('_')], [], [stx(false)]}]).

cguard(Cs) ->
    Var = stx('I'),
    [cop(Var, C) || C <- Cs].

cop(Var, {C1, C2}) ->
    [stx(infix, {stx(C1), '=<', Var}),
     stx(infix, {Var, '=<', stx(C2)})];
cop(Var, {C0}) when is_integer(C0)->
    [stx(infix, {stx(C0), '=:=', Var})].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syntax manipulation

-define(IS_TREE(T), tree =:= element(1, T)).
-define(IS_UPC(I), $A =< I, I =< $Z; $_ == I).
-define(IS_CHAR(I), $ =< I, I =< $~).
%% auto detect type (for var, int, and atom)
stx(T) when ?IS_TREE(T) ->
    T;
stx(A) when is_atom(A) ->
    stx(atom_to_list(A));
stx([I|_] = S) when ?IS_UPC(I) ->
    stx(var, S);
stx([I|_] = S) when ?IS_CHAR(I) ->
    stx(atom, S);
stx(I) when is_integer(I) ->
    stx(integer, I).

%% aliases
stx(bin_fields, Vs) ->
    [stx(bin_field, V) || V <- Vs];
stx(clauses, Cs) ->
    [stx(clause, {Args, G, Body}) || {Args, G, Body} <- Cs];
stx(func, {Name, Args, G, Body}) ->
    stx(func, {Name, stx(clauses, [{Args, G, Body}])});
stx(list, L) when is_list(L) ->
    stx(list, {L, none});
stx(rule_fun, N) when is_atom(N) ->
    stx(implicit_fun, {stx(rule, N), 1});
stx(rule, N) ->
    [$-|atom_to_list(N)]++"-";

%% primitives
stx(atom, A) ->
    erl_syntax:atom(A);
stx(bin, Vs) ->
    erl_syntax:binary(stx(bin_fields, Vs));
stx(bin_field, V) ->
    erl_syntax:binary_field(stx(V));
stx(integer, I) when is_integer(I) ->
    erl_syntax:integer(I);
stx(list, {H, T}) when is_list(H) ->
    erl_syntax:list(H, T);
stx(tuple, L) when is_list(L) ->
    erl_syntax:tuple(L);

%% expressions
stx(call, {F, Args}) ->
    erl_syntax:application(stx(F), Args);
stx(call, {M, F, Args}) ->
    erl_syntax:application(stx(mf, {M, F}), Args);
stx(case_, {Arg, Clauses}) ->
    erl_syntax:case_expr(Arg, Clauses);
stx(eq, {L, R}) ->
    erl_syntax:match_expr(L, R);
stx(fun_, Cs) ->
    erl_syntax:fun_expr(stx(clauses, Cs));
stx(infix, {Left, Op, Right}) ->
    erl_syntax:infix_expr(Left, stx(op, Op), Right);
stx(mf, {M, F}) ->
    erl_syntax:module_qualifier(stx(M), stx(F));
stx(var, V) ->
    erl_syntax:variable(V);

%% syntax elements
stx(attr, {N, As}) ->
    erl_syntax:attribute(stx(N), As);
stx(clause, {Args, Guard, Body}) ->
    erl_syntax:clause(Args, Guard, Body);
stx(fa, {N, A}) ->
    erl_syntax:arity_qualifier(stx(N), stx(A));
stx(func, {Name, Clauses}) when is_list(Clauses)->
    erl_syntax:function(stx(Name), Clauses);
stx(implicit_fun, {N, A}) ->
    erl_syntax:implicit_fun(stx(fa, {N, A}));
stx(op, Op) ->
    erl_syntax:operator(Op);

%% utilities
stx(fold, {F, A, Forms}) ->
    erl_syntax_lib:fold(F, A, Forms);
stx(mktuple, X) ->
    erl_syntax:make_tree(tuple, [X]);
stx(set_pos, {Tree, Line}) ->
    erl_syntax:set_pos(Tree, Line);
stx(rev, Forms) ->
    erl_syntax:revert(Forms);
stx(revert, Forms) ->
    erl_syntax:revert_forms(Forms).
