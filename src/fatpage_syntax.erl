%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_syntax).

-export(
   [stx/1,
    set_pos/2,
    local/2,
    revert/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Syntax manipulation

-define(IS_TREE(T), tree =:= element(1, T)).
-define(IS_UPC(I), $A =< I, I =< $Z; $_ =:= I).
-define(IS_CHAR(I), $ =< I, I =< $~).

%% we are idempotent
stx(T) when ?IS_TREE(T) ->
    T;

%% auto detect type (for var, int, bin, and atom)
stx(I) when is_integer(I) ->
    stx({integer, I});
stx(F) when is_float(F) ->
    stx({float, F});
stx([I|_] = S) when ?IS_UPC(I) ->
    stx({var, S});
stx([I|_] = S) when ?IS_CHAR(I) ->
    stx({atom, S});
stx(L) when is_list(L) ->
    [stx(E) || E <- L];
stx(A) when is_atom(A) ->
    stx(atom_to_list(A));
stx(B) when is_binary(B) ->
    stx(binary_to_list(B));

%% aliases
stx({bin_fields, Vs}) ->
    [stx({bin_field, V}) || V <- Vs];
stx({clauses, AGBs}) ->
    [stx({clause, AGB}) || AGB <- AGBs];
stx({func, {Name, Args, G, Body}}) ->
    stx({func, {Name, stx({clauses, [{Args, G, Body}]})}});    
stx({record, {Name, Fields}}) ->
    stx({attr, {record, [Name, {list, Fields}]}});
stx({utf8, Bin}) when is_binary(Bin) ->
    stx({bin, [stx({integer, I}) || I <- binary_to_list(Bin)]});

%% primitives
stx({atom, A}) ->
    erl_syntax:atom(A);
stx({bin, Vs}) ->
    erl_syntax:binary(stx({bin_fields, stx(Vs)}));
stx({cons, H, T}) ->
    erl_syntax:cons(stx(H), stx(T));
stx({integer, I}) when is_integer(I) ->
    erl_syntax:integer(I);
stx({list, {H, T}}) when is_list(H) ->
    erl_syntax:list(stx(H), stx(T));
stx({list, L}) when is_list(L) ->
    erl_syntax:list(stx(L));
stx({tuple, L}) when is_list(L) ->
    erl_syntax:tuple(stx(L));

%% expressions
stx({call, {F, Args}}) ->
    erl_syntax:application(stx(F), stx(Args));
stx({call, {M, F, Args}}) ->
    erl_syntax:application(stx({mf, {M, F}}), stx(Args));
stx({case_, {Arg, Clauses}}) ->
    erl_syntax:case_expr(stx(Arg), stx(Clauses));
stx({eq, {L, R}}) ->
    erl_syntax:match_expr(stx(L), stx(R));
stx({fun_, Cs}) ->
    erl_syntax:fun_expr(stx({clauses, Cs}));
stx({infix, {Left, Op, Right}}) ->
    erl_syntax:infix_expr(stx(Left), stx({op, Op}), stx(Right));
stx({mf, {M, F}}) ->
    erl_syntax:module_qualifier(stx(M), stx(F));
stx({var, V}) ->
    erl_syntax:variable(V);

%% syntax elements
stx({field, Name}) ->
    erl_syntax:record_field(stx(Name));
stx({attr, {N, As}}) ->
    erl_syntax:attribute(stx(N), stx(As));
stx({bin_field, V}) ->
    erl_syntax:binary_field(stx(V));
stx({clause, {Args, Guard, Body}}) ->
    erl_syntax:clause(stx(Args), stx(Guard), stx(Body));
stx({fa, {N, A}}) ->
    erl_syntax:arity_qualifier(stx(N), stx(A));
stx({func, {Name, Clauses}}) when is_list(Clauses)->
    erl_syntax:function(stx(Name), stx(Clauses));
stx({imp_fun, {N, A}}) ->
    erl_syntax:implicit_fun(stx({fa, {N, A}}));
stx({op, Op}) ->
    erl_syntax:operator(Op).

%% utilities
%%stx({mktuple, X}) ->
%%    erl_syntax:make_tree(tuple, [X]);

local(calls, Forms) -> fold(fun local_calls/2, [], Forms);
local(funcs, Forms) -> fold(fun local_funcs/2, [], Forms).

fold(Fun, Acc, T) when is_tuple(T) -> fold(Fun, Fun(T, Acc), tuple_to_list(T));
fold(Fun, Acc, L) when is_list(L) -> lists:foldl(folder(Fun), Fun(L, Acc), L);
fold(Fun, Acc, X) -> Fun(X, Acc).

folder(Fun) ->
    fun(E, A) -> fold(Fun, Fun(E, A), E) end.

set_pos(Stx, Line) ->
    erl_syntax:set_pos(Stx, Line).

revert(Forms) ->
    [erl_syntax:revert(F) || F <- Forms].

local_calls(Tree, O) ->
    try is_tuple(Tree) andalso erl_syntax:type(Tree) of
        application ->
            App = erl_syntax:application_operator(Tree),
            Arity = length(erl_syntax:application_arguments(Tree)),
            case erl_syntax:type(App) of
                atom -> [{erl_syntax:atom_value(App), Arity}|O];
                _ -> O
            end;
        implicit_fun ->
            N = erl_syntax:implicit_fun_name(Tree),
            [{erl_syntax:atom_value(erl_syntax:arity_qualifier_body(N)),
             erl_syntax:integer_value(erl_syntax:arity_qualifier_argument(N))}|O];
        _ -> O
    catch
        _:_ -> O
    end.

local_funcs(Tree, O) ->
    try is_tuple(Tree) andalso erl_syntax:type(Tree) of
        function ->
            [{erl_syntax:atom_value(erl_syntax:function_name(Tree)),
              erl_syntax:function_arity(Tree)}|O];
        _ -> O
    catch
        _:_ -> O
    end.
