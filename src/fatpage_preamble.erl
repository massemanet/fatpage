%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_preamble).

-export([file/2, string/2,
        repeat/4, alternative/2, sequence/2, final/2, final/3]).

-export(['-EOF-'/1, '-CR-'/1]).

-record(obj, {ptr, bin, sz}).

'-EOF-'(#obj{ptr = Ptr, sz = Z} = Obj) ->
    if Z == Ptr -> {ok, eof, Obj#obj{ptr = eof}};
       Ptr < Z -> {error, not_eof};
       true -> {error, past_eof}
    end.

'-CR-'(Obj) ->
    final(10, Obj).

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
        {error, R} -> {error, R}
    end.

repeat(Min, Max, F, Obj) ->
    repeat(0, Min, minus1(Max), F, Obj, []).

repeat(N, Mn, Mx, F, Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} when N =:= Mx -> {ok, lists:reverse([X|Xs]), O};
        {ok, X, O}               -> repeat(N+1, Mn, Mx, F, O, [X|Xs]);
        {error, R} when N < Mn   -> {error, {too_few, R}};
        {error, _R}              -> {ok, lists:reverse(Xs), Obj}
    end.

minus1(infinity) -> infinity;
minus1(I) -> I-1.

alternative(_, #obj{ptr = eof}) ->
    {error, eof};
alternative(Fs, Obj) ->
    alternative(Fs, Obj, []).

alternative([], _Obj, Es) ->
    {error, {fail, Es}};
alternative([F|Fs], Obj, Es) ->
    case F(Obj) of
        {ok, X, O} -> {ok, X, O};
        {error, E} -> alternative(Fs, Obj, [E|Es])
    end.

sequence(_, #obj{ptr = eof}) ->
    {error, eof};
sequence(Fs, Obj) -> 
    sequence(Fs, Obj, []).

sequence([], Obj, Xs) ->
    {ok, lists:reverse(Xs), Obj};
sequence([F|Fs], Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} -> sequence(Fs, O, [X|Xs]);
        {error, R} -> {error, {unexpected, R}}
    end.

final(Char, Obj) when is_integer(Char) ->
    case peek_chars(Obj, 1) of
        {[Char], Bin, Sz} -> {ok, Bin, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E}}
    end.
final(C1, C2, Obj) when is_integer(C1), is_integer(C2) ->
    case peek_chars(Obj, 1) of
        {[C], Bin, Sz} when C1 =< C, C =< C2 ->
            {ok, Bin, bump_ptr(Obj, Sz)};
        E ->
            {error, {miss, E, C1, C2}}
    end.

peek_chars(#obj{ptr = Ptr, sz = Sz}, Num) when Sz < Ptr+Num ->
    {error, eof};
peek_chars(#obj{bin = Bin, ptr = Ptr}, 1) ->
    case binary:part(Bin, {Ptr, 1}) of
        <<0:1, C0:7>> = B ->
            {[C0], B, 1};
        <<6:3, C1:5>> = B1 ->
            case binary:part(Bin, {Ptr+1, 1}) of
                <<2:2,C2:6>> = B2 ->
                    {[(C1 bsl 6)+C2], <<B1/binary, B2/binary>>, 2};
                B2 ->
                    error({unhandled_utf8, B1, B2})
            end;
        B0 ->
            error({unhandled_utf8, B0})
    end;
peek_chars(#obj{bin = Bin, ptr = Ptr}, Num) ->
    try unicode:characters_to_list(B = binary:part(Bin, {Ptr, Num})) of
        Chars when Num =:= length(Chars) -> {Chars, B, Num};
        {incomplete, H, T} -> error({unhandled_utf8, H, T});
        Chars -> error({unhandled_utf8, Chars})
    catch
        error:badarg -> {error, {miss, eof}}
    end.

bump_ptr(#obj{ptr = Ptr} = Obj, N) -> Obj#obj{ptr = Ptr+N}.
