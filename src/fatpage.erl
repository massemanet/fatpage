%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage).

-export(
   [go/1]).

%ex() -> [{repeat, 1, 2, {sequence, [$A, {$B,$C}, {alternative, [$E, $G]}]}}].
% matches "AB", "AC", "ABAB", "ABAC", "ACAB", "ACAC"

go(B) ->
    Z = byte_size(B),
    case s1(0, B) of
        {ok, Xs, Z} -> {ok, Xs, 0};
        {ok, Xs, P} -> {ok, Xs, Z-P};
        {error, R, Z} -> {error, R, eof};
        {error, R, P} -> {error, R, P}
    end.

literal(C, Ptr, B) ->
    Z = byte_size(C),
    try binary:part(B, {Ptr, Z}) of
        C -> {ok, C, Ptr+Z};
        E -> {error, {miss, E, C}, Ptr}
    catch
        error:badarg -> {error, {miss, eof, C}, Ptr}
    end.

literal(C1, C2, Ptr, B) ->
    try binary:part(B, {Ptr, 1}) of
        C when C1 =< C, C =< C2 -> {ok, C, Ptr+1};
        E -> {error, {miss, E, C1, C2}, Ptr}
    catch
        error:badarg -> {error, {miss, eof, C1, C2}, Ptr}
    end.

repeat(Min, Max, F, Ptr, B) ->
    repeat(0, Min, Max-1, F, Ptr, B, []).

repeat(N, Min, Mx, F, Ptr, B, Xs) ->
    case F(Ptr, B) of
        {ok, X, P} when N == Mx    -> {ok, lists:reverse([X|Xs]), P};
        {ok, X, P}                 -> repeat(N+1, Min, Mx, F, P, B, [X|Xs]);
        {error, R, P} when N < Min -> {error, {too_few, R}, P};
        {error, _R, _P}            -> {ok, lists:reverse(Xs), Ptr}
    end.

sequence(Fs, Ptr, B) -> 
    sequence(Fs, Ptr, B, []).

sequence([], Ptr, _B, Xs) ->
    {ok, lists:reverse(Xs), Ptr};
sequence([F|Fs], Ptr, B, Xs) ->
    case F(Ptr, B) of
        {ok, X, P} -> sequence(Fs, P, B, [X|Xs]);
        {error, R, P} -> {error, {unexpected, R}, P}
    end.

alternative(Fs, Ptr, B) ->
    alternative(Fs, Ptr, B, []).

alternative([], Ptr, _, Es) -> {error, {fail, Es}, Ptr};
alternative([F|Fs], Ptr, B, Es) ->
    case F(Ptr, B) of
        {ok, X, P} -> {ok, X, P};
        {error, R, P} -> alternative(Fs, Ptr, B, [{R, P}|Es])
    end.

s1(Ptr, B) ->
    repeat(1, 2, fun s2/2, Ptr, B).

s2(Ptr, B) ->
    sequence([fun s3/2, fun s4/2, fun s5/2], Ptr, B).

s3(Ptr, B) ->
    literal(<<"A">>, Ptr, B).

s4(Ptr, B) ->
    literal(<<"B">>, <<"C">>, Ptr, B).

s5(Ptr, B) ->
    alternative([fun s6/2, fun s7/2], Ptr, B).

s6(Ptr, B) ->
    literal(<<"E">>, Ptr, B).

s7(Ptr, B) ->
    literal(<<"G">>, Ptr, B).
