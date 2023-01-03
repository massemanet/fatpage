%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code gen (rules -> forms)

-module(fatpage_gen).

-export(
   [forms/2]).

-record(rule, {name, deriv, code}).
-record(deriv, {type, x, ds}).

forms(Mod, Rules) ->
    Forms = gen_forms(Mod, Rules),
    PreAmble = preamble_forms(),
    resolve(Forms, PreAmble).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code generator from rules

gen_forms(Mod, Rules) ->
    #rule{name = FirstRule} = hd(Rules),
    Header = header(Mod, FirstRule),
    {Forms, _} = lists:mapfoldl(fun rule/2, 1, Rules),
    Header++Forms.

rule(#rule{name=Name, code=Code, deriv=#deriv{type=Type, x=X, ds=SubDeriv}}, Num) ->
    {rule(Num, Name, Code, Type, X, SubDeriv), Num+1}.

header(Mod, Entry) ->
    IF = {imp_fun, {rule_name(Entry), 1}},
    FN = 'Filename',
    ST = 'String',
    stx([{attr, {module, [Mod]}},
         {attr, {export, [{list, [{fa, {file, 1}}, {fa, {string, 1}}]}]}},
         {func, {file, [FN], [], [{call, {file, [FN, IF]}}]}},
         {func, {string, [ST], [], [{call, {string, [ST, IF]}}]}}]).

rule(Line, Name, Code, Type, X, Sub) ->
    stx_pos(rule(Name, Code, Type, X, Sub), Line).

rule(Name, Code, Type, X, Sub) ->
    {func, {rule_name(Name), ['Obj'], [], [rule_body(Code, Type, X, Sub)]}}.

rule_body(undefined, Type, X, Sub) -> fcall(Type, X, Sub);
rule_body(Code, Type, X, Sub) -> fcase(Code, Type, X, Sub).

fcase(Code, Type, X, Deriv) ->
    Nbindings = nbindings(Type, Deriv),
    {case_, {fcall(Type, X, Deriv), [fclause(ok, Nbindings, Code),
                                     fclause(err, 0, 'Err')]}}.

-define(DERIV(Type, X, Ds), {deriv, Type, X, Ds}).
fcall(rep, {Min, Max}, Deriv) ->
    {call, {repeat, [Min, Max, final(Deriv), 'Obj']}};
fcall(alt, {}, Derivs) ->
    {call, {alternative, [finals(Derivs), 'Obj']}};
fcall(seq, {}, Derivs) ->
    {call, {sequence, [finals(Derivs), 'Obj']}};
fcall(final, char, Cs) ->
    call_final(Cs, 'Obj');
fcall(final, appl, Name) ->
    {call, {Name, ['Obj']}}.

fclause(ok, Nbindings, Code) ->
    Match = {tuple, [ok, bindings(Nbindings, Code), 'O']},
    Ret = {tuple, [ok, Code, 'O']},
    {clause, {[Match], [], [Ret]}};
fclause(err, _, V) ->
    {clause, {[V], [], [V]}}.

nbindings(rep, _) -> 1;
nbindings(alt, _) -> 1;
nbindings(seq, Derivs) -> length(Derivs).

bindings(Nbindings, Code) ->
    BoundVars = bound_vars(Code),
    AvailVars = avail_vars(Nbindings),
    Wild = match_var(0),
    case {BoundVars -- [Wild], BoundVars -- AvailVars} of
        {[], [Wild]} -> Wild;
        {BoundVars, []} -> match_vars(BoundVars, AvailVars);
        {BoundVars, [Wild]} -> {eq, {match_vars(BoundVars, AvailVars), Wild}};
        _ -> error({unknown_vars, {BoundVars, AvailVars, Wild}})
    end.

bound_vars(Code) ->
    lists:usort(bound_vars(Code, [])).

bound_vars({var, Var}, Vs) ->
    [Var|Vs].

avail_vars(N) ->
    [match_var(I) || I <- lists:seq(1, N)].

match_var(0) ->
    'Y';
match_var(I) ->
    list_to_atom("Y"++integer_to_list(I)).

match_vars(BoundVars, AvailVars) ->
    {list, [match_var(V, BoundVars) || V <- AvailVars]}.

match_var(V, Vs) ->
    case lists:member(V, Vs) of
        true -> V;
        false -> '_'
    end.

finals(Derivs) ->
    {list, [final(Deriv) || Deriv <- Derivs]}.

final(#deriv{type=final, x=char, ds=C}) -> cfun([C]);
final(#deriv{type=final, x=appl, ds=Name}) -> {imp_fun, {rule_name(Name), 1}}.

cfun([X]) ->
    {fun_, [{['O'], [], [call_final(X, 'O')]}]}.

call_final({char, C}, Var) ->
    {call, {final, [{utf8, utf8(C)}, Var]}};
call_final({range, C1, C2}, Var) ->
    {call, {final, [{tuple, [{utf8, utf8(C1)}, {utf8, utf8(C2)}]}, Var]}}.

rule_name(N) ->
    lists:flatten(io_lib:format("-~s-", [N])).

utf8(Char) ->
    unicode:characters_to_binary([Char]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse preamble

preamble_forms() ->
    {ok, Forms} = epp:parse_file(preamble(), []),
    lists:filtermap(fun function_kv/1, Forms).

preamble() ->
    case filelib:wildcard("**/fatpage_preamble.erl") of
        [Preamble|_] -> Preamble;
        [] -> error({cant_find_preamble})
    end.

-define(FUNCTION(N, A), {function, _, N, A, _}).
-define(RECORD(N, Fs), {attribute, _, record, {N, Fs}}).
function_kv(?FUNCTION(N, A) = F) -> {true, {{N, A}, F}};
function_kv(?RECORD(N, Fs)) -> {true, {record, {N, [field_name(F) || F <- Fs]}}};
function_kv(_) -> false.

field_name({record_field, _, {atom, _, Name}}) ->
    Name.

records(PreAmble) ->
    stx(lists:filter(fun is_record/1, PreAmble)).

is_record(F) when element(1, F) == record -> true;
is_record(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% resolve generated code from preamble

resolve(Fs, PreambleFunctions) ->
    case local_calls(Fs) -- local_funcs(Fs) of
        [] ->
            Fs;
        Unknown ->
            Resolved = do_resolve(Unknown, PreambleFunctions, []),
            resolve(Fs++Resolved, PreambleFunctions)
    end.

do_resolve([], Preamble, Resolved) ->
    records(Preamble)++lists:reverse(Resolved);
do_resolve([U|Unknown], PreambleFunctions, Resolved) ->
    case lists:filter(fun({function, _, N, A, _}) -> {N, A} == U end, Resolved) of
        [_] ->
            do_resolve(Unknown, PreambleFunctions, Resolved);
        [] ->
            case proplists:get_value(U, PreambleFunctions, undefined) of
                undefined -> error({unresolved, U});
                Form -> do_resolve(Unknown, PreambleFunctions, [Form|Resolved])
            end
    end.

local_calls(Fs) ->
    lists:usort(stx_local(calls, Fs)).

local_funcs(Fs) ->
    stx_local(funcs, Fs) ++ builtin_functions().

builtin_functions() ->
    erlang:module_info(exports).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% aliases

stx_local(What, Forms) ->
    fatpage_syntax:local(What, Forms).

stx_pos(Form, Line) ->
    fatpage_syntax:set_pos(
      fatpage_syntax:stx(Form), Line).

%%stx_rev(Forms) ->
%%    fatpage_syntax:rev(Forms).

stx(Forms) ->
    fatpage_syntax:stx(Forms).
