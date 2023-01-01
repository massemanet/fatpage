%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% This is not a callable module. The loader includes functions to
%%% resolve calls from here at compile time.

-module(fatpage_preamble).

%% API
-export(
   [file/2,
    string/2]).

%% contructor primitives
-export(
   [atomize/1,
    squeeze/1]).

%% combinators (exported to keep compiler happy)
-export(
   [alternative/2,
    final/2,
    repeat/4,
    sequence/2]).

%% core rules
-export(
   ['-ALPHA-'/1,
    '-BIT-'/1,
    '-CR-'/1,
    '-CRLF-'/1,
    '-DIGIT-'/1,
    '-DQUOTE-'/1,
    '-HEXDIG-'/1,
    '-HTAB-'/1,
    '-LF-'/1,
    '-SP-'/1,
    '-VCHAR-'/1,
    '-WSP-'/1]).

%% extra builtin rules
-export(
   ['-EOF-'/1,
    '-WS-'/1]).

-record(obj, {ptr, bin, sz}).

'-ALPHA-'(Obj) ->  % A-Z / a-z
    case peek_chars(Obj, 1) of
        {C, 1} when <<$A>> =< C, C =< <<$Z>>;
                    <<$a>> =< C, C =< <<$z>> -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, alpha, C}};
        Err -> Err
    end.

'-BIT-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$0>> =:= C;
                    <<$1>> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, bit, C}};
        Err -> Err
    end.

'-CR-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$\n>> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, cr, C}};
        Err -> Err
    end.

'-CRLF-'(Obj) ->
    case peek_chars(Obj, 2) of
        {C, 2} when <<$\n, $\r>> =:= C -> {ok, C, bump_ptr(Obj, 2)};
        {C, 2} -> {error, {miss, crlf, C}};
        Err -> Err
    end.

'-DIGIT-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$0>> =< C, C =< <<$9>> -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, digit, C}};
        Err -> Err
    end.

'-DQUOTE-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$\">> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, dquote, C}};
        Err -> Err
    end.

'-HEXDIG-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$A>> =< C, C =< <<$F>>;
                    <<$a>> =< C, C =< <<$f>>;
                    <<$0>> =< C, C =< <<$9>> -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, hexdig, C}};
        Err -> Err
    end.

'-HTAB-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$\t>> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, htab, C}};
        Err -> Err
    end.
    
'-LF-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$\t>> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, lf, C}};
        Err -> Err
    end.

%% '-LWSP-'(Obj)           =  *(WSP / CRLF WSP)
%% '-OCTET-'(Obj)          =  %x00-FF

'-SP-'(Obj) ->
    case peek_chars(Obj, 1) of
        {C, 1} when <<$ >> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, sp, C}};
        Err -> Err
    end.

'-VCHAR-'(Obj) -> % visible (printing) characters
    case peek_chars(Obj, 1) of
        {C, 1} when <<$!>> =< C, C =< <<$~>> -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, vchar, C}};
        Err -> Err
    end.

'-WSP-'(Obj) -> % whitespace
    '-WS-'(Obj).

'-EOF-'(#obj{ptr = Ptr, sz = Z} = Obj) ->
    if Z == Ptr -> {ok, eof, Obj#obj{ptr = eof}};
       Ptr < Z -> {error, not_eof};
       true -> {error, past_eof}
    end.

'-WS-'(Obj) -> % whitespace
    case peek_chars(Obj, 1) of
        {C, 1} when <<$ >> =:= C;
                    <<$\t>> =:= C -> {ok, C, bump_ptr(Obj, 1)};
        {C, 1} -> {error, {miss, ws, C}};
        Err -> Err
    end.

squeeze(L) when is_list(L) ->
    binary:list_to_bin(L);
squeeze(X) ->
    X.

atomize(X) ->
    binary_to_atom(squeeze(X)).

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

final(Bin, Obj) when is_binary(Bin) ->
    case peek_chars(Obj, byte_size(Bin)) of
        {Bin, Sz} -> {ok, Bin, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E, Bin}}
    end.

peek_chars(#obj{ptr = Ptr, sz = Sz}, Num) when Sz < Ptr + Num ->
    {error, eof};
peek_chars(#obj{bin = Bin, ptr = Ptr}, 2) ->
    case binary:part(Bin, {Ptr, 2}) of
        <<0:1, _:7, 0:1, _:7>> = B -> {B, 2};
        B -> error({unhandled_utf8, B})
    end;
peek_chars(#obj{bin = Bin, ptr = Ptr}, 1) ->
    case binary:part(Bin, {Ptr, 1}) of
        <<0:1, _:7>> = B -> {B, 1};
        <<6:3, _:5>> = B1 ->
            case binary:part(Bin, {Ptr+1, 1}) of
                <<2:2, _:6>> = B2 -> {<<B1/binary, B2/binary>>, 2};
                B2 -> error({unhandled_utf8, B1, B2})
            end;
        B0 -> error({unhandled_utf8, B0})
    end.

bump_ptr(#obj{ptr = Ptr} = Obj, N) -> Obj#obj{ptr = Ptr+N}.
