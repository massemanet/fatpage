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
    numerize/2,
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

'-EOF-'(#{ptr := Ptr, sz := Sz} = Obj) ->
    if Sz == Ptr -> {ok, eof, Obj#{ptr => eof}};
       Ptr < Sz -> {error, not_eof};
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

numerize(hex, N) when is_list(N) ->
    erlang:list_to_integer(binary_to_list(squeeze(N)), 16).

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
    Obj = #{ptr => 0, bin => B, sz => byte_size(B)},
    case F(Obj) of
        {ok, Xs, #{ptr := eof}} -> {ok, Xs, eof};
        {ok, Xs, #{sz := Sz, ptr := Ptr}} -> {ok, Xs, Sz-Ptr};
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

minus1(inf) -> inf;
minus1(I) -> I-1.

alternative(_, #{ptr := eof}) ->
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

sequence(_, #{ptr := eof}) ->
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

final({B1, B2}, Obj) when is_binary(B1), is_binary(B2) ->
    Sz = byte_size(B1),
    case Sz =:= byte_size(B2) andalso peek_chars(Obj, Sz) of
        {B0, Sz} when B1 =< B0, B0 =< B2 -> {ok, B0, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E, {B1, B2}}}
    end;
final(Bin, Obj) when is_binary(Bin) ->
    case peek_chars(Obj, byte_size(Bin)) of
        {Bin, Sz} -> {ok, Bin, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E, Bin}}
    end.

peek_chars(#{ptr := Ptr, sz := Sz}, Num) when Sz < Ptr + Num ->
    {error, eof};
peek_chars(#{bin := Bin, ptr := Ptr}, N) ->
    {binary:part(Bin, {Ptr, N}), N}.

bump_ptr(Obj, N) ->
    maps:update_with(ptr, fun(Ptr) -> Ptr+N end, Obj).
