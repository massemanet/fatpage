%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_preamble).

-export([file/2, string/2,
        repeat/4, alternative/2, sequence/2, final/2]).

-export(['-EOF-'/1, '-CR-'/1]).

-record(obj, {ptr, bin, sz}).

'-EOF-'(#obj{ptr = Ptr, sz = Z} = Obj) ->
    if Z == Ptr -> {ok, eof, eof};
       Ptr < Z -> {error, not_eof, Obj};
       true -> {error, past_eof, eof}
    end.

'-CR-'(Obj) ->
    final(<<10>>, Obj).

file(Filename, F) ->
    case file:read_file(Filename) of
        {ok, B} -> string(B, F);
        {error, R} -> error({unreadable, R})
    end.

string(L, F) when is_list(L) ->
    string(list_to_binary(L), F);
string(B, _) when not is_binary(B) ->
    error({badarg, not_a_string});
string(B, F) ->
    Obj = #obj{ptr = 0, bin = B, sz = byte_size(B)},
    case F(Obj) of
        {ok, Xs, O} when O#obj.ptr =:= eof -> {ok, Xs, eof};
        {ok, Xs, O} when O#obj.sz =:= O#obj.ptr -> {ok, Xs, 0};
        {ok, Xs, O} -> {ok, Xs, O#obj.sz-O#obj.ptr};
        {error, R, #obj{ptr = P}} -> {error, R, P}
    end.

-define(IS_EOF(O), O#obj.ptr =:= eof).
%% should F return O in {error, R, O}?

repeat(Min, Max, F, Obj) ->
    repeat(0, Min, minus1(Max), F, Obj, []).

repeat(N, Min, Mx, F, Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} when N =:= Mx   -> {ok, lists:reverse([X|Xs]), O};
        {ok, X, O}                 -> repeat(N+1, Min, Mx, F, O, [X|Xs]);
        {error, R, O} when N < Min -> {error, {too_few, R}, O};
        {error, _R, O}             -> {ok, lists:reverse(Xs), O}
    end.

minus1(infinity) -> infinity;
minus1(I) -> I-1.

alternative(_, Obj) when ?IS_EOF(Obj) ->
    {error, eof, Obj};
alternative(Fs, Obj) ->
    alternative(Fs, Obj, []).

alternative([], Obj, Es) ->
    {error, {fail, Es}, Obj};
alternative([F|Fs], Obj, Es) ->
    case alt(F, Obj) of
        {ok, X, O} -> {ok, X, O};
        {error, R, O} -> alternative(Fs, Obj, [{R, O}|Es])
    end.

alt(F, Obj) ->
    case erlang:fun_info(F, arity) of
        {arity, 1} -> final(F, Obj);
        {arity, 2} -> F(Obj)
    end.

sequence(_, Obj) when ?IS_EOF(Obj) ->
    {error, eof, Obj};
sequence(Fs, Obj) -> 
    sequence(Fs, Obj, []).

sequence([], Obj, Xs) ->
    {ok, lists:reverse(Xs), Obj};
sequence([F|Fs], Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} -> sequence(Fs, O, [X|Xs]);
        {error, R, O} -> {error, {unexpected, R}, O}
    end.

final(Bin, Obj) when is_binary(Bin) ->
    try peek_chars(Obj, Z = byte_size(Bin)) of
        Bin -> {ok, Bin, bump_ptr(Obj, Z)};
        E -> {error, {miss, E}, Obj}
    catch
        error:badarg -> {error, {miss, eof}, Obj}
    end;
final({Bin1, Bin2}, Obj) ->
    try binary:part(Obj#obj.bin, {Ptr = Obj#obj.ptr, 1}) of
        Bin when Bin1 =< Bin, Bin =< Bin2 -> {ok, Obj#obj{ptr = Ptr+1}};
        E -> {error, {miss, E, Bin1, Bin2}, Obj}
    catch
        error:badarg -> {error, {miss, eof, Bin1, Bin2}, Obj}
    end;
final(GUARD, Obj) when is_function(GUARD, 1) ->
    try GUARD(C = peek_chars(Obj, 1)) of
        true -> {ok, C, bump_ptr(Obj, 1)};
        false -> {error, {miss, C}, Obj}
    catch
        error:badarg -> {error, {miss, eof}, Obj}
    end.

peek_chars(#obj{bin = Bin, ptr = Ptr}, Num) ->
    binary:part(Bin, {Ptr, Num}).

bump_ptr(Obj = #obj{ptr = Ptr}, N) ->
    Obj#obj{ptr = Ptr+N}.
