-module(fatpage_gen).

-export(
   [parse/1,
    r2r/1,
    unroll/1,
    left_recursive/1,
    forms/2,
    pp/2]).

-define(DBG(Form, E),
        try (E)
        catch C:R -> erlang:display({C, R, Form}),"\n :( \n"
        end).

pp(Forms, Filename) ->
    comment(Filename)++lists:flatmap(fun ppf/1, Forms).

forms(Mod, Rules) ->
    gen_forms(Mod, Rules).

left_recursive(Rules) ->
    fixpoint({Rules, predefined()}).

unroll(Rules) ->
    erase(fatpage),
    unroll(Rules, []).

r2r(Rules) ->
    lists:map(fun reify/1, Rules).

parse(String) when is_list(String) ->
    r2r(parse_abnf(String)).

parse_abnf(String) ->
    Parsers = [fatpage_rfc5234, fatpage_bootstrap_abnf, abnfc_rfc4234],
    case lists:filter(fun is_available/1, Parsers) of
        [] -> error({no_parser});
        [M|_] -> parse(M, String)
    end.

is_available(Mod) ->
    [] =/= [M || {M, _, _} <- code:all_available(), M =:= atom_to_list(Mod)].

parse(abnfc_rfc4234, String) ->
    {ok, {rulelist, undefined, Rules}, []} = abnfc_rfc4234:decode(rulelist, String),
    Rules;
parse(Mod, String) ->
    {ok, Rs, 0} = Mod:string(String),
    Rs.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(deriv, {type, x = {}, ds}).
-record(rule, {name, deriv, code = <<>>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% rewrite bootstrap parser output to internal form

reify(D) when is_record(D, deriv) -> D;
reify(Ds) when is_list(Ds) -> lists:map(fun reify/1, Ds);

%% fatpage_bootstrap terms
reify({rule, N, D, C}) -> #rule{name = N, code = C, deriv = reify(D)};
reify({seq, Seqs})     -> #deriv{type = seq, ds = reify(Seqs)};
reify({alt, Alts})     -> #deriv{type = alt, ds = reify(Alts)};
reify({rep, Rep, D})   -> #deriv{type = rep, x = Rep, ds = reify(D)};
reify({app, App})      -> #deriv{type = final, x = appl, ds = App};
reify({chs, Chs})      -> #deriv{type = final, x = char, ds = Chs};

%% abnfc terms
reify({rule, def_rule, N, D, C})  -> #rule{name = N, code = C, deriv = reify(D)};
reify({rulename, Name})           -> #deriv{type = final, x = appl, ds = Name};
reify({char_alt, Alts})           -> #deriv{type = alt, ds = reify(Alts)};
reify({char_seq, Seqs})           -> #deriv{type = seq, ds = reify(Seqs)};
reify({repeat, Min, Max, Rep})    -> #deriv{type = rep, x = {Min, Max}, ds = reify(Rep)};
reify({char_val, C0})             -> #deriv{type = final, x = char, ds = <<C0:8>>};
reify({char_range, C1, C2})       -> #deriv{type = final, x = char, ds = {<<C1:8>>, <<C2:8>>}}.

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
%% prettyprinter

ppf(Form) ->
    ?DBG(Form, erl_prettypr:format(Form, [{paper, 100}, {ribbon, 200}])++[10, 10]).

comment(Filename) ->
    flat("%%% This file was generated by fatpage from ~s.~n"
         "%%% It happened on ~s, at ~s. It was a ~s.~n"
         "%%% You should probably not modify it.~n~n",
         [Filename, fdate(), ftime(), fday()]).

fdate() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    flat("~.4.0w-~.2.0w-~.2.0w", [Y, M, D]).

ftime() ->
    {_, {H, M, S}} = calendar:universal_time(),
    flat("~.2.0w:~.2.0w:~.2.0w", [H, M, S]).

fday() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    case calendar:day_of_the_week(Y, M, D) of
        1 -> "Monday";
        2 -> "Tuesday";
        3 -> "Wednesday";
        4 -> "Thursday";
        5 -> "Friday";
        6 -> "Saturday";
        7 -> "Sunday"
    end.

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
    [
     {'ALPHA', non_nullable},
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code gen

gen_forms(Mod, Rules) ->
    Forms = fatpage_g:forms(Mod, Rules),
    PreAmble = preamble_forms(),
    resolve(Forms++records(PreAmble), PreAmble).

preamble_forms() ->
    {ok, Forms} = epp:parse_file(preamble(), []),
    fatpage_g:fold_forms(fun function_kv/2, [], Forms).

preamble() ->
    case filelib:wildcard("**/fatpage_preamble.erl") of
        [Preamble|_] -> Preamble;
        [] -> error({cant_find_preamble})
    end.

-define(FUNCTION(N, A), {function, _, N, A, _}).
-define(RECORD(N), {attribute, _, record, {N, _}}).
function_kv(?FUNCTION(N, A) = F, B) -> [{{N, A}, F}|B];
function_kv(?RECORD(N) = R, B) -> [{{record, N}, R}|B];
function_kv(_, B) -> B.

records(PreAmble) ->
    lists:filtermap(fun is_record/1, PreAmble).

is_record({{record, _}, Rec}) -> {true, Rec};
is_record(_) -> false.

resolve(Fs, PreambleFunctions) ->
    case lists:filter(mk_is_unknown(Fs), local_calls(Fs)) of
        [] ->
            Fs;
        Unknown ->
            Resolved = do_resolve(Unknown, PreambleFunctions, []),
            resolve(Fs++Resolved, PreambleFunctions)
    end.

do_resolve([], _, Resolved) ->
    lists:reverse(Resolved);
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

mk_is_unknown(Fs) ->
    KnownFunctions = known_functions(Fs),
    fun(FA)-> not lists:member(FA, KnownFunctions) end.

local_calls(Fs) ->
    fatpage_g:fold_forms(fun local_calls/2, [], Fs).

local_calls({call, _, {atom, _, N}, Ps}, B) -> [{N, length(Ps)}|B];
local_calls({'fun', _, {function, N, A}}, B) -> [{N, A}|B];
local_calls(_, B) -> B.

known_functions(Fs) ->
    local_functions(Fs) ++ builtin_functions().

local_functions(Fs) ->
    lists:foldl(fun local_function/2, [], Fs).

local_function(?FUNCTION(N, A), B) -> [{N, A}|B];
local_function(_, B) -> B.

builtin_functions() ->
    erlang:module_info(exports).
