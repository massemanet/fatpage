-module(fatpage).

-export(
   [file/2,
    string/2]).
-export(
   [final/3,
    repeat/5,
    sequence/3,
    alternative/3,
    merge/1]).

file(Filename, F) ->
    case file:read_file(Filename) of
        {ok, B} -> string(B, F);
        {error, R} -> error({unreadable, R})
    end.

string(L, F) when is_list(L) -> string(list_to_binary(L), F);
string(B, _) when not is_binary(B) -> error({badarg, not_a_string});
string(B, F) ->
    Z = byte_size(B),
    case F(0, B) of
        {ok, Xs, Z} -> {ok, Xs, 0};
        {ok, Xs, P} -> {ok, Xs, Z-P};
        {error, R, Z} -> {error, R, eof};
        {error, R, P} -> {error, R, P}
    end.

final(Bin, Ptr, B) when is_binary(Bin) ->
    Z = byte_size(Bin),
    try binary:part(B, {Ptr, Z}) of
        Bin -> {ok, Bin, Ptr+Z};
        E -> {error, {miss, E, Bin}, Ptr}
    catch
        error:badarg -> {error, {miss, eof, Bin}, Ptr}
    end;
final({Bin1, Bin2}, Ptr, B) ->
    try binary:part(B, {Ptr, 1}) of
        Bin when Bin1 =< Bin, Bin =< Bin2 -> {ok, Bin, Ptr+1};
        E -> {error, {miss, E, Bin1, Bin2}, Ptr}
    catch
        error:badarg -> {error, {miss, eof, Bin1, Bin2}, Ptr}
    end;
final(GUARD, Ptr, B) when is_function(GUARD, 1) ->
    try GUARD(C = binary:part(B, {Ptr, 1})) of
        true -> {ok, C, Ptr+1};
        false -> {error, {miss, C}, Ptr}
    catch
        error:badarg -> {error, {miss, eof}, Ptr}
    end.

repeat(Min, Max, F, Ptr, B) ->
    repeat(0, Min, minus1(Max), F, Ptr, B, byte_size(B), []).

repeat(N, Min, Mx, F, Ptr, B, Sz, Xs) ->
    case F(Ptr, B) of
        {ok, X, P} when N =:= Mx   -> {ok, lists:reverse([X|Xs]), P};
        {ok, X, P}                 -> repeat(N+1, Min, Mx, F, P, B, Sz, [X|Xs]);
        {error, R, P} when N < Min -> {error, {too_few, R}, P};
        {error, _R, P}             -> {ok, lists:reverse(Xs), P}
    end.

minus1(infinity) -> infinity;
minus1(I) -> I-1.

merge([]) -> [];
merge(Xs) when is_binary(hd(Xs)) -> binary:list_to_bin(Xs);
merge(Xs) when is_list(hd(Xs)) -> lists:append(Xs).

sequence(_, Ptr, B) when byte_size(B) =< Ptr ->
    {error, eof, Ptr};
sequence(Fs, Ptr, B) -> 
    sequence(Fs, Ptr, B, []).

sequence([], Ptr, _B, Xs) ->
    {ok, lists:reverse(Xs), Ptr};
sequence([F|Fs], Ptr, B, Xs) ->
    case F(Ptr, B) of
        {ok, X, P} -> sequence(Fs, P, B, [X|Xs]);
        {error, R, P} -> {error, {unexpected, R}, P}
    end.

alternative(_, Ptr, B) when byte_size(B) =< Ptr ->
    {error, eof, Ptr};
alternative(Fs, Ptr, B) ->
    alternative(Fs, Ptr, B, []).

alternative([], Ptr, _, Es) ->
    {error, {fail, Es}, Ptr};
alternative([F|Fs], Ptr, B, Es) ->
    case alt(F, Ptr, B) of
        {ok, X, P} -> {ok, X, P};
        {error, R, P} -> alternative(Fs, Ptr, B, [{R, P}|Es])
    end.

alt(F, Ptr, B) ->
    case erlang:fun_info(F, arity) of
        {arity, 1} -> final(F, Ptr, B);
        {arity, 2} -> F(Ptr, B)
    end.
