-module(fatpage_xpr).

-export([go/1]).

go(Text) ->
    p(root, s(Text)).

-define(TRY(Type, Exp, S, X), try {hit, Type(Exp, S)} catch throw:X -> {miss, X} end).
-define(MEMO(Rule, Type, Exp, S, X),
        case memo(read, Rule, S, S) of
            undefined -> memo(preload, Rule, S, S),
                         memo(write, Rule, ?TRY(Type, Exp, S, X), S);
            {miss, X} -> throw(X);
            X -> X
        end).

p(root, S) ->
    ?MEMO(root, alt, [[expr, "+", expr], expr], S, X);
p([expr, "+", expr], S) ->
    seq([expr, "+", expr], S);
p(expr, S) ->
    ?MEMO(expr, alt, [num, ["(", expr, ")"], expr], S, X);
p(["(", expr, ")"], S) ->
    seq(["(", expr, ")"], S);
p("+", S) ->
    ?MEMO("+", chr, "+", S, X);
p("(", S) ->
    ?MEMO("(", chr, "(", S, X);
p(")", S) ->
    ?MEMO(")", chr, ")", S, X);
p(num, S) ->
    ?MEMO(num, p, "0-9", S, X);
p("0-9", S) ->
    ?MEMO("0-9", range, "0-9", S, X).

%% memoizer
memo(read, Rule, #{pos := Pos}, S) ->
    case get({fatpage, Rule, Pos}) of
        undefined -> undefined;
        {miss, R} -> {miss, R};
        {Pos, Parse} -> {Parse, s(pos, Pos, S)}
    end;
memo(preload, Rule, #{pos := Pos}, S) ->
    put({fatpage, Rule, Pos}, {miss, preload}),
    {preload, S};
memo(write, Rule, {miss, R}, #{pos := Pos}) ->
    put({fatpage, Rule, Pos}, {miss, R}),
    throw(R);
memo(write, Rule, {hit, {Parse, S}}, #{pos := Pos}) ->
    put({fatpage, Rule, Pos}, {maps:get(pos, S), Parse}),
    {Parse, S}.

%% alternatives
alt(Alts, S) ->
    try lt(Alts, S)
    catch throw:_ -> throw({no_alt, Alts})
    end.

lt([], S) ->
    throw({no_alt, S});
lt([Alt|Alts], S) ->
    try p(Alt, S)
    catch throw:_ -> lt(Alts, S)
    end.

seq(Ps, S0) ->
    [S|Ys] = lists:foldl(fun sq/2, [S0], Ps),
    {lists:reverse(Ys), S}.

sq(Seq, [S0|O]) ->
    {T, S} = p(Seq, S0),
    [S, T|O].

chr([Chr], S0) ->
    case s({peek, 1}, S0) of
        {Chr, S} -> {[Chr], S};
        _ -> throw({missed_char, [Chr]})
    end.

range("0-9", S0) ->
    case s({peek, 1}, S0) of
        {Chr, S} when $0 =< Chr, Chr =< $9 -> {[Chr], S};
        _ -> throw({missed_range, ["0-9"]})
    end.

%% constructor
s(Text) ->
    #{text => Text, size => length(Text), pos => 1}.

%% getter
s(pos, #{pos := Pos}) ->
    Pos;
s({peek, Sz}, S = #{text := Text, size := Size, pos := Pos}) ->
    case Pos+Sz-1 =< Size andalso lists:nth(Pos, Text) of
        false -> {eof, S};
        R -> {R, S#{pos => Pos+Sz}}
    end.

%% setter
s(Key, Val, S) ->
    maps:update(Key, Val, S).
