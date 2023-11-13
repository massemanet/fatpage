-module(permutp).

-export [go/1].

%%% The grammar
%%%
%%% root -> expr
%%% expr -> term aop term => {2, 1, 3}
%%%       | term
%%% term -> fact mop fact => {2, 1, 3}
%%%       | fact
%%% fact -> '(' expr ')' => 2
%%%       | lit
%%% aop  -> '+'
%%%       | '-'
%%% mop  -> '*' 
%%%       | '/'
%%% lit  -> num
%%%       | var
%%% num  -> /0|[1-9][0-9]*/
%%% var  -> /[A-Z]/

%%% 'p' is the parser rules, generated from the grammar.
p(root, S) ->
    p(expr, S);
p(expr, S) ->
    alt([[term, aop, term], term], S);
p([term, aop, term], S) ->
    seq([term, aop, term], fun([A, B, C]) -> {B, A, C} end, S);
p(term, S) ->
    alt([[fact, mop, fact], fact], S);
p([fact, mop, fact], S) ->
    seq([fact, mop, fact], fun([A, B, C]) -> {B, A, C} end, S);
p(fact, S) ->
    alt([[<<"(">>, expr, <<")">>], lit], S);
p([<<"(">>, expr, <<")">>], S) ->
    seq([<<"(">>, expr, <<")">>], fun([_, B, _]) -> B end, S);
p(aop, S) ->
    alt([<<"+">>, <<"-">>], S);
p(mop, S) ->
    alt([<<"*">>, <<"/">>], S);
p(lit, S) ->
    alt([num, var], S);
p(num, S) ->
    re(num, S);
p(var, S) ->
    re(var, S);
p(<<"(">>, S) ->
    str(<<"(">>, S);
p(<<")">>, S) ->
    str(<<")">>, S);
p(<<"+">>, S) ->
    str(<<"+">>, S);
p(<<"-">>, S) ->
    str(<<"-">>, S);
p(<<"*">>, S) ->
    str(<<"*">>, S);
p(<<"/">>, S) ->
    str(<<"/">>, S);
p(T, S) ->
    throw({T, S}).

%%% The regexps we will use.
regexps() ->
    #{num => new_re("0|[1-9][0-9]*"),
      var => new_re("[A-Z]")}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The engine. Not generated.

go(Text) ->
    try {Tree, _S} = p(root, s(Text)), Tree
    catch throw:R -> error({top, R})
    end.

%% alternatives
alt([], S) ->
    throw({no_alt, S});
alt([Alt|Alts], S) ->
    try p(Alt, S)
    catch throw:_ -> alt(Alts, S)
    end.

%% sequences
seq(Seqs, Ret, S0) ->
    [S|Ys] = lists:foldl(fun seq/2, [S0], Seqs),
    {Ret(Ys), S}.

seq(Seq, [S0|O]) ->
    {T, S} = p(Seq, S0),
    [S, T|O].

%%% regexp literals
new_re(RE) ->
    case re:compile(RE, [anchored, no_auto_capture]) of
        {ok, C} -> C;
        E -> error({illegal_regexp, {RE, E}})
    end.

re(Key, S) ->
    #{Key := RE} = s(re, S),
    Text = s(text, S),
    Size = s(size, S),
    Pos = s(pos, S),
    Opts = [{offset, Pos}, {capture, first, binary}],
    case Pos =< Size andalso re:run(Text, RE, Opts) of
        {match, [M]} -> {M, s(pos, Pos+byte_size(M), S)};
        nomatch -> throw({nomatch, Pos})
    end.

%%% string literals.
str(Str, S) ->
    Text = s(text, S),
    Size = s(size, S),
    Pos = s(pos, S),
    Sz = byte_size(Str),
    case Pos+Sz =< Size andalso binary:part(Text, Pos, Sz) of
        Str -> {Str, s(pos, Pos+Sz, S)};
        Z -> throw({nomatch, {Str, Z}})
    end.

%%% 's' is the state. constructor, getter, setter,
s(Text) ->
    T = to_str(Text),
    #{text => T, size => byte_size(T), pos => 0, re => regexps()}.
s(Key, S) ->
    maps:get(Key, S).
s(Key, Val, S) ->
    S#{Key => Val}.

to_str(L) when is_list(L) -> list_to_binary(L);
to_str(B) when is_binary(B) -> B.
