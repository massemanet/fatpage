%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(csv).

-export(
   ['_file'/2,
    file/1,
    string/1]).

-define(GUARD(G), fun(<<I>>) when G -> true; (_) -> false end).

file(Filename) -> fatpage:file(Filename, fun '_file'/2).
string(String) -> fatpage:string(String, fun '_file'/2).

'_file'(Ptr, B) ->
    fatpage:sequence([fun '_record'/2, fun '_eol-records'/2, fun '_eols'/2], Ptr, B).

'_eol-records'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_eol-record'/2, Ptr, B).

'_eol-record'(Ptr, B) ->
    fatpage:sequence([fun '_eol'/2, fun '_record'/2], Ptr, B).

'_eols'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_eol'/2, Ptr, B).

'_eol'(Ptr, B) ->
    fatpage:alternative([fun '_EOLC'/2, fun '_CRLF'/2], Ptr, B).

'_record'(Ptr, B) ->
    fatpage:sequence([fun '_field'/2, fun '_comma_fields'/2], Ptr, B).

'_comma_fields'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_comma-field'/2, Ptr, B).

'_comma-field'(Ptr, B) ->
    fatpage:sequence([fun '_COMMA'/2, fun '_field'/2], Ptr, B).

'_field'(Ptr, B) ->
    fatpage:alternative([fun '_escaped'/2, fun '_non-escaped'/2], Ptr, B).

'_escaped'(Ptr, B) ->
    fatpage:sequence([fun '_DQUOTE'/2, fun '_qchars'/2, fun '_DQUOTE'/2], Ptr, B).

'_qchars'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_qchar'/2, Ptr, B).

'_non-escaped'(Ptr, B) ->
    fatpage:repeat(0, infinity, fun '_TEXTDATA'/2, Ptr, B).

'_qchar'(Ptr, B) ->
    fatpage:alternative([fun '_DQCHAR'/2, fun '_NQCHAR'/2], Ptr, B).

'_DQCHAR'(Ptr, B) ->
    fatpage:literal(<<"\"\"">>, Ptr, B).

-define(NQCHAR(), $  =< I, I =< $!; $# =< I, I =< $~).
'_NQCHAR'(Ptr, B) ->
    fatpage:literal(?GUARD(?NQCHAR()), Ptr, B).

-define(TEXTDATA(), $  =< I, I =< $!; $# =< I, I =< $+; $- =< I, I =< $~).
'_TEXTDATA'(Ptr, B) ->
    fatpage:literal(?GUARD(?TEXTDATA()), Ptr, B).

'_COMMA'(Ptr, B) ->
    fatpage:literal(<<",">>, Ptr, B).

'_DQUOTE'(Ptr, B) ->
    fatpage:literal(<<"\"">>, Ptr, B).

-define(EOLC(), 10 ==I; 13 == I).
'_EOLC'(Ptr, B) ->
    fatpage:literal(?GUARD(?EOLC()), Ptr, B).

'_CRLF'(Ptr, B) ->
    fatpage:literal(<<"\n\r">>, Ptr, B).
