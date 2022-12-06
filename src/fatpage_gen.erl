-module(fatpage_gen).

-export([parse/1,
         unroll/1,
         left_recursive/1,
         forms/1,
         pp/1,
         erl/1,
         display/1,
         beam/1]).

-define(DBG(E), try (E) catch C:R -> erlang:display({C, R, F}),"\n :( \n" end).

beam(Filename) ->
    OutFile = out_file(Filename, beam),
    {ok, _Mod, Beam} = compile:forms(forms(Filename)),
    file:write_file(OutFile, Beam),
    OutFile.

display(Filename) ->
    io:fwrite("~s", [pp(Filename)]).

erl(Filename) ->
    OutFile = out_file(Filename, erl),
    Erl = pp(Filename),
    write_file(OutFile, Erl),
    OutFile.

pp(Filename) ->
    lists:flatmap(fun ppf/1, forms(Filename)).

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
    flat("-virtual-~p-", [I]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prettyprinter

ppf(F) ->
    ?DBG(erl_prettypr:format(F, [{paper, 100}, {ribbon, 200}])++[10, 10]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% code gen

mod(Filename) ->
    filename:basename(Filename, ".abnf").

gen_forms(Mod, Rules) ->
    Forms = fatpage_g:forms(Mod, Rules),
    PreAmble = preamble_forms(),
    resolve(Forms++records(PreAmble), PreAmble).

preamble_forms() ->
    {ok, Forms} = epp:parse_file("src/fatpage_preamble.erl",[]),
    fatpage_g:fold_forms(fun function_kv/2, [], Forms).

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

fix([], Final, ORs)                             -> {lists:usort(ORs), lists:usort(Final)};
fix([?RULE(Name, _, Deriv)|Rs], Final, ORs)     -> fix([{Name, Deriv}|Rs], Final, ORs);
fix([{Name, V}|Rs], Final, ORs) when is_atom(V) -> fix(Rs, [{Name, V}|Final], ORs);
fix([{Name, Deriv}|Rs], Final, ORs)             -> fix(Rs, Final, [{Name, type(Deriv, Final)}|ORs]).

type(non_nullable, _)                  -> non_nullable;
type(nullable, _)                      -> nullable;
type(?DERIV(rep, {0, _}, _), _)        -> nullable;
type(?DERIV(rep, _, D), _)             -> D;
type(?DERIV(final, char, _), _)        -> non_nullable;
type(?DERIV(final, appl, 'EOF'), _)    -> non_nullable;
type(?DERIV(final, appl, Name), Final) -> rewrite_name(Name, Final);
type(?DERIV(alt, _, As), Final)        -> rewrite_alt(As, Final);
type(?DERIV(seq, _, Ss), Final)        -> rewrite_seq(Ss, Final).

rewrite_name(Name, Final) ->
    case proplists:get_value(Name, Final, no) of
        no -> ?DERIV(final, appl, Name);
        V -> V
    end.

rewrite_alt(As, Final) ->
    case is_nullable(As) of
        no -> non_nullable;
        yes -> nullable;
        perhaps -> ?DERIV(alt, {}, recurse(As, Final))
    end.

rewrite_seq([], _) -> nullable;
rewrite_seq(Ss, Final) ->
    case hd(Ss) of
        non_nullable -> non_nullable;
        nullable -> ?DERIV(seq, {}, tl(Ss));
        _ -> ?DERIV(seq, {}, recurse(Ss, Final))
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate beam and rite .beam file

out_file(Filename, Ext) ->
    Dir = filename:dirname(Filename),
    Base = filename:basename(Filename, ".abnf"),
    flat("~s/~s.~s", [Dir, Base, Ext]).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

write_file(OutFile, String) ->
    {ok, FD} = file:open(OutFile, [write]),
    io:fwrite(FD, "~s", [String]),
    file:close(FD).

