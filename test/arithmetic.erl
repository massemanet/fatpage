%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(arithmetic).

-export(
   [file/1,
    string/1]).

%ex() -> [{repeat, 1, 2, {sequence, [$A, {$B,$C}, {alternative, [$E, $G]}]}}].
% matches "AB", "AC", "ABAB", "ABAC", "ACAB", "ACAC"

file(Filename) ->
    case file:read_file(Filename) of
        {ok, B} -> string(B);
        {error, R} -> error({unreadable, R})
    end.

string(L) when is_list(L) -> string(list_to_binary(L));
string(B) when not is_binary(B) -> error({badarg, not_a_string});
string(B) ->
    Z = byte_size(B),
    case s1(0, B) of
        {ok, Xs, Z} -> {ok, Xs, 0};
        {ok, Xs, P} -> {ok, Xs, Z-P};
        {error, R, Z} -> {error, R, eof};
        {error, R, P} -> {error, R, P}
    end.

s1(Ptr, B) ->
    fatpage:repeat(1, 2, fun s2/2, Ptr, B).

s2(Ptr, B) ->
    fatpage:sequence([fun s3/2, fun s4/2, fun s5/2], Ptr, B).

s3(Ptr, B) ->
    fatpage:literal(<<"A">>, Ptr, B).

s4(Ptr, B) ->
    fatpage:literal(<<"B">>, <<"C">>, Ptr, B).

s5(Ptr, B) ->
    fatpage:alternative([fun s6/2, fun s7/2], Ptr, B).

s6(Ptr, B) ->
    fatpage:literal(<<"E">>, Ptr, B).

s7(Ptr, B) ->
    fatpage:literal(<<"G">>, Ptr, B).
