%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(csv).

-export(
   ['_file'/2,
    file/1,
    string/1]).

-define(GUARD(G), fun(<<I>>) when G -> true; (_) -> false end).

file(Filename) -> fatpage:file(Filename, fun '_file'/2).

string(String) -> fatpage:string(String, fun '_file'/2).

-define(DBG(P, B), erlang:display({?FUNCTION_NAME, try binary:part(B,{P,1}) catch _:_ -> eof end})).

'_file'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_record-eol'/2, Ptr, B).

'_record-eol'(Ptr, B) ->
    case fatpage:sequence([fun '_record'/2, fun '_eol'/2], Ptr, B) of
        {ok, [Y1, _], P} -> {ok, Y1, P};
        Err -> Err
    end.

'_record'(Ptr, B) ->
    case fatpage:sequence([fun '_field'/2, fun '_comma_fields'/2], Ptr, B) of
        {ok, [Y1, Y2], P} -> {ok, [Y1|Y2], P};
        Err -> Err
    end.

'_eol'(Ptr, B) ->
    fatpage:alternative([fun '_CR'/2, fun '_LF'/2, fun '_CRLF'/2], Ptr, B).

'_comma_fields'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_comma-field'/2, Ptr, B).

'_comma-field'(Ptr, B) ->
    case fatpage:sequence([fun '_COMMA'/2, fun '_field'/2], Ptr, B) of
        {ok, [_, Y2], P} -> {ok, Y2, P};
        Err -> Err
    end.

'_field'(Ptr, B) ->
    fatpage:alternative([fun '_quoted'/2, fun '_uqtexts'/2], Ptr, B).

'_quoted'(Ptr, B) ->
    case fatpage:sequence([fun '_WSs'/2, fun '_DQUOTE'/2, fun '_qtexts'/2, fun '_DQUOTE'/2, fun '_WSs'/2], Ptr, B) of
        {ok, [_, _, Y3, _, _], P} -> {ok, Y3, P};
        Err -> Err
    end.

'_qtexts'(Ptr, B) ->
    case fatpage:repeat(0, infinity, fun '_QTEXT'/2, Ptr, B) of
        {ok, Xs, P} -> {ok, fatpage:merge(Xs), P};
        Err -> Err
    end.

'_QTEXT'(Ptr, B) ->
    fatpage:alternative([fun '_UQTEXT'/2, fun '_COMMA'/2], Ptr, B).

'_uqtexts'(Ptr, B) ->
    case fatpage:repeat(0, infinity, fun '_UQTEXT'/2, Ptr, B) of
        {ok, Xs, P} -> {ok, fatpage:merge(Xs), P};
        Err -> Err
    end.

'_UQTEXT'(Ptr, B) ->
    fatpage:alternative([fun '_UNICODEISH'/2, fun '_WS'/2, fun '_DOUBLEDQ'/2], Ptr, B).

'_WSs'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_WS'/2, Ptr, B).

'_DOUBLEDQ'(Ptr, B) ->
    case fatpage:literal(<<"\"\"">>, Ptr, B) of
        {ok, _, P} -> {ok, <<"\"">>, P};
        Err -> Err
    end.

'_CRLF'(Ptr, B) ->
    fatpage:literal(<<"\n\r">>, Ptr, B).

'_CR'(Ptr, B) ->
    fatpage:literal(<<"\n">>, Ptr, B).

'_LF'(Ptr, B) ->
    fatpage:literal(<<"\r">>, Ptr, B).

-define(WS(), $\t == I; $  == I).
'_WS'(Ptr, B) ->
    fatpage:literal(?GUARD(?WS()), Ptr, B).

'_DQUOTE'(Ptr, B) ->
    fatpage:literal(<<"\"">>, Ptr, B).

'_COMMA'(Ptr, B) ->
    fatpage:literal(<<",">>, Ptr, B).

-define(UNICODEISH(), $! == I; $# =< I, I =< $+; $- =< I, I =< $~; 16#80 =< I, I =< 16#7FF).
'_UNICODEISH'(Ptr, B) ->
    fatpage:literal(?GUARD(?UNICODEISH()), Ptr, B).
