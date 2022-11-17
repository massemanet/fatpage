-module(fatpage_gen).

-export([parse/1,
         left_recursive/1]).

parse(Filename) ->
    {ok, B} = file:read_file(Filename),
    {rulelist, _, Rules} = abnfc:parse(binary_to_list(B), []),
    Rules.

left_recursive(Filename) ->
    fixpoint({parse(Filename), []}).

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

type(non_nullable, _)      -> non_nullable;
type(nullable, _)          -> nullable;
type({char_val, _}, _)     -> non_nullable;
type({char_range, _, _}, _)-> non_nullable;
type({repeat, 0, _, _}, _) -> nullable;
type({repeat, _, _, R}, _) -> R;
type({rulename, R}, Final) -> rewrite_rulename(R, Final);
type({alt, As}, Final)     -> rewrite_alt(As, Final);
type({seq, Ss}, Final)     -> rewrite_seq(Ss, Final).

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
