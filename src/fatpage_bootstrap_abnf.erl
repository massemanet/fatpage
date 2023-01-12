%%% This is the fatpage rfc5234 bootstrap parser. It is based on a
%%% file generated by fatpage from src/rfc5234.abnf.  It happened on
%%% 2022-12-15, at 14:48:10. It was a Thursday.  You should probably
%%% not modify it.

-module(fatpage_bootstrap_abnf).

-export([file/1, string/1]).

file(Filename) -> file(Filename, fun '-rulelist-'/1).

string(String) -> string(String, fun '-rulelist-'/1).

'-rulelist-'(Obj) -> repeat(1, inf, fun '--virtual-0--'/1, Obj).

'--virtual-0--'(Obj) -> alternative([fun '-rule-'/1, fun '--virtual-1--'/1], Obj).

'--virtual-1--'(Obj) -> sequence([fun '--virtual-2--'/1, fun '-c-nl-'/1], Obj).

'--virtual-2--'(Obj) -> repeat(0, inf, fun '-c-wsp-'/1, Obj).

'-rule-'(Obj) ->
    case sequence([fun '-rulename-'/1, fun '-defined-as-'/1, fun '-elements-'/1, fun '-c-nl-'/1], Obj) of
        {ok, Y, O} -> {ok, {rule, Y}, O};
        Err -> Err
    end.

'-rulename-'(Obj) ->
    case sequence([fun '-ALPHA-'/1, fun '--virtual-3--'/1], Obj) of
        {ok, [Y1, Y2], O} -> {ok, atomize([Y1|Y2]), O};
        Err -> Err
    end.

'--virtual-3--'(Obj) -> repeat(0, inf, fun '--virtual-4--'/1, Obj).

'--virtual-4--'(Obj) ->
    alternative([fun '-ALPHA-'/1, fun '-DIGIT-'/1, fun (O) -> final(45, O) end], Obj).

'-defined-as-'(Obj) ->
    case sequence([fun '--virtual-2--'/1, fun '--virtual-5--'/1, fun '--virtual-2--'/1], Obj) of
        {ok, [_, Y2, _], O} -> {ok, atomize(Y2), O};
        Err -> Err
    end.

'--virtual-5--'(Obj) -> alternative([fun (O) -> final(61, O) end, fun '--virtual-6--'/1], Obj).

'--virtual-6--'(Obj) ->
    case sequence([fun (O) -> final(61, O) end, fun (O) -> final(47, O) end], Obj) of
        {ok, [_, Y2, _], O} -> {ok, squeeze(Y2), O};
        Err -> Err
    end.

'-elements-'(Obj) ->
    case sequence([fun '-alternation-'/1, fun '--virtual-2--'/1], Obj) of
        {ok, [Y1, _], O} -> {ok, Y1, O};
        Err -> Err
    end.

'-c-wsp-'(Obj) -> alternative([fun '-WSP-'/1, fun '--virtual-7--'/1], Obj).

'--virtual-7--'(Obj) -> sequence([fun '-c-nl-'/1, fun '-WSP-'/1], Obj).

'-c-nl-'(Obj) -> alternative([fun '-construct-'/1, fun '-comment-'/1, fun '-CRLF-'/1], Obj).

'-construct-'(Obj) ->
    case sequence([fun (O) -> final(59, O) end,
                   fun(O) -> final(59, O) end,
                   fun '--virtual-8--'/1,
                   fun '-CRLF-'/1], Obj) of
        {ok, [_, _, Y3, _], O} -> {ok, {construct, [squeeze(Y3)]}, O};
        Err -> Err
    end.

'-comment-'(Obj) ->
    case sequence([fun (O) -> final(59, O) end, fun '--virtual-8--'/1, fun '-CRLF-'/1], Obj) of
        {ok, [_, Y2, _], O} -> {ok, {comment, squeeze(Y2)}, O};
        Err -> Err
    end.

'--virtual-8--'(Obj) -> repeat(0, inf, fun '--virtual-9--'/1, Obj).

'--virtual-9--'(Obj) -> alternative([fun '-WSP-'/1, fun '-VCHAR-'/1], Obj).

'-alternation-'(Obj) ->
    case sequence([fun '-concatenation-'/1, fun '--virtual-10--'/1], Obj) of
        {ok, [Y1, Y2], O} -> {ok, {alt, [Y1|Y2]}, O};
        Err -> Err
    end.

'--virtual-10--'(Obj) -> repeat(0, inf, fun '-ws-concat-'/1, Obj).

'-ws-concat-'(Obj) ->
    case sequence([fun '--virtual-2--'/1,
                   fun (O) -> final(47, O) end,
                   fun '--virtual-2--'/1,
                   fun '-concatenation-'/1], Obj) of
        {ok, [_, _, _, Y4], O} -> {ok, Y4, O};
        Err -> Err
    end.

'-concatenation-'(Obj) ->
    case sequence([fun '-repetition-'/1, fun '--virtual-12--'/1], Obj) of
        {ok, [Y1, Y2], O} -> {ok, {seq, [Y1|Y2]}, O};
        Err -> Err
    end.

'--virtual-12--'(Obj) -> repeat(0, inf, fun '-ws-rep-'/1, Obj).

'-ws-rep-'(Obj) ->
    case sequence([fun '--virtual-14--'/1, fun '-repetition-'/1], Obj) of
        {ok, [_, Y2], O} -> {ok, Y2, O};
        Err -> Err
    end.

'--virtual-14--'(Obj) -> repeat(1, inf, fun '-c-wsp-'/1, Obj).

'-repetition-'(Obj) ->
    case sequence([fun '--virtual-15--'/1, fun '-element-'/1], Obj) of
        {ok, [Y1, Y2], O} -> {ok, {rep, Y1, Y2}, O};
        Err -> Err
    end.

'--virtual-15--'(Obj) ->
    repeat(0, 1, fun '-repeat-'/1, Obj).

'-repeat-'(Obj) -> alternative([fun '-rep-star-'/1, fun '-rep-dig-'/1], Obj).

'-rep-star-'(Obj) ->
    case sequence([fun '--virtual-18--'/1,
                   fun (O) -> final(42, O) end,
                   fun '--virtual-18--'/1], Obj) of
        {ok, [Y1, _, Y3], O} -> {ok, {if Y1==[] -> 0; true -> numerize(Y1, 10) end,
                                      if Y3==[] -> inf; true -> numerize(Y3, 10) end}, O};
        Err -> Err
    end.

'-rep-dig-'(Obj) ->
    case repeat(1, inf, fun '-DIGIT-'/1, Obj) of
        {ok, Y, O} -> {ok, {numerize(Y, 10), numerize(Y, 10)}, O};
        Err -> Err
    end.

'--virtual-18--'(Obj) -> repeat(0, inf, fun '-DIGIT-'/1, Obj).

'-element-'(Obj) ->
    alternative([fun '-appl-'/1,
                 fun '-group-'/1,
                 fun '-option-'/1,
                 fun '-val-'/1],
                Obj).

'-appl-'(Obj) ->
    case '-rulename-'(Obj) of
        {ok, Y, O} -> {ok, {app, atomize(Y)}, O};
        Err -> Err
    end.

'-group-'(Obj) ->
    case sequence([fun (O) -> final(40, O) end,
                   fun '--virtual-2--'/1,
                   fun '-alternation-'/1,
                   fun '--virtual-2--'/1,
                   fun (O) -> final(41, O) end],
                  Obj) of
        {ok, [_, _, Y3, _, _], O} -> {ok, Y3, O};
        Err -> Err
    end.

'-option-'(Obj) ->
    case sequence([fun (O) -> final(91, O) end,
                   fun '--virtual-2--'/1,
                   fun '-alternation-'/1,
                   fun '--virtual-2--'/1,
                   fun (O) -> final(93, O) end],
                  Obj) of
        {ok, [_, _, Y3, _, _], O} -> {ok, {rep, [{0, 1}], Y3}, O};
        Err -> Err
    end.

'-val-'(Obj) ->
    alternative([fun '-char-val-'/1,
                 fun '-num-val-'/1,
                 fun '-prose-val-'/1],
                Obj).

'-char-val-'(Obj) ->
    case sequence([fun '-DQUOTE-'/1, fun '--virtual-19--'/1, fun '-DQUOTE-'/1], Obj) of
        {ok, [_, Y2, _], O} -> {ok, Y2, O};
        Err -> Err
    end.

'--virtual-19--'(Obj) -> repeat(0, inf, fun '--virtual-20--'/1, Obj).

'--virtual-20--'(Obj) ->
    alternative([fun (O) -> final({32, 33}, O) end, fun (O) -> final({35, 126}, O) end], Obj).

'-num-val-'(Obj) ->
    case sequence([fun (O) -> final(37, O) end, fun '--virtual-21--'/1], Obj) of
        {ok, [_, Y2], O} -> {ok, Y2, O};
        Err -> Err
    end.

'--virtual-21--'(Obj) ->
    alternative([fun '-bin-val-'/1, fun '-dec-val-'/1, fun '-hex-val-'/1], Obj).

'-bin-val-'(Obj) ->
    sequence([fun '--virtual-22--'/1, fun '--virtual-23--'/1, fun '--virtual-24--'/1], Obj).

'--virtual-22--'(Obj) ->
    alternative([fun (O) -> final(98, O) end, fun (O) -> final(66, O) end], Obj).

'--virtual-23--'(Obj) -> repeat(1, inf, fun '-BIT-'/1, Obj).

'--virtual-24--'(Obj) -> repeat(0, 1, fun '--virtual-25--'/1, Obj).

'--virtual-25--'(Obj) -> alternative([fun '--virtual-26--'/1, fun '--virtual-27--'/1], Obj).

'--virtual-26--'(Obj) -> repeat(1, inf, fun '--virtual-28--'/1, Obj).

'--virtual-27--'(Obj) -> sequence([fun (O) -> final(45, O) end, fun '--virtual-23--'/1], Obj).

'--virtual-28--'(Obj) -> sequence([fun (O) -> final(46, O) end, fun '--virtual-23--'/1], Obj).

'-dec-val-'(Obj) ->
    sequence([fun '--virtual-29--'/1, fun '-rep-dig-'/1, fun '--virtual-30--'/1], Obj).

'--virtual-29--'(Obj) ->
    alternative([fun (O) -> final(100, O) end, fun (O) -> final(68, O) end], Obj).

'--virtual-30--'(Obj) -> repeat(0, 1, fun '--virtual-31--'/1, Obj).

'--virtual-31--'(Obj) -> alternative([fun '--virtual-32--'/1, fun '--virtual-33--'/1], Obj).

'--virtual-32--'(Obj) -> repeat(1, inf, fun '--virtual-34--'/1, Obj).

'--virtual-33--'(Obj) -> sequence([fun (O) -> final(45, O) end, fun '-rep-star-'/1], Obj).

'--virtual-34--'(Obj) -> sequence([fun (O) -> final(46, O) end, fun '-rep-star-'/1], Obj).

'-hex-val-'(Obj) ->
    case sequence([fun '--virtual-35--'/1,
                   fun '--virtual-36--'/1,
                   fun '--virtual-37--'/1], Obj) of
        {ok, [_, Y2, Y3], O} -> {ok, if Y3 == [] -> {char, numerize(Y2, 16)};
                                        true -> {numerize(Y2, 16), hd(Y3)}
                                     end, O};
        Err -> Err
    end.

'--virtual-35--'(Obj) ->
    alternative([fun (O) -> final(120, O) end, fun (O) -> final(88, O) end], Obj).

'--virtual-36--'(Obj) -> repeat(1, inf, fun '-HEXDIG-'/1, Obj).

'--virtual-37--'(Obj) -> repeat(0, 1, fun '-hex-suffix-'/1, Obj).

'-hex-suffix-'(Obj) -> alternative([fun '--virtual-39--'/1, fun '-hex-suffix-dash-'/1], Obj).
    
'--virtual-39--'(Obj) -> repeat(1, inf, fun '-hex-suffix-dot-'/1, Obj).

'-hex-suffix-dot-'(Obj) ->
    case sequence([fun (O) -> final(46, O) end, fun '--virtual-36--'/1], Obj) of
        {ok, [_, Y2], O} -> {ok, {dot, numerize(Y2, 16)}, O};
        Err -> Err
    end.

'-hex-suffix-dash-'(Obj) ->
    case sequence([fun (O) -> final(45, O) end, fun '--virtual-36--'/1], Obj) of
        {ok, [_, Y2], O} -> {ok, {dash, numerize(Y2, 16)}, O};
        Err -> Err
    end.

'-prose-val-'(Obj) ->
    sequence([fun (O) -> final(60, O) end, fun '--virtual-42--'/1, fun (O) -> final(62, O) end],
             Obj).

'--virtual-42--'(Obj) -> repeat(0, inf, fun '--virtual-43--'/1, Obj).

'--virtual-43--'(Obj) ->
    alternative([fun (O) -> final({32, 61}, O) end, fun (O) -> final({63, 126}, O) end], Obj).

-record(obj, {ptr, bin, sz}).

file(Filename, F) ->
    case file:read_file(Filename) of
        {ok, B} -> string(B, F);
        {error, R} -> error({unreadable, R})
    end.

string(L, F) when is_list(L) -> string(list_to_binary(L), F);
string(B, _) when not is_binary(B) -> error({badarg, not_a_string});
string(B, F) ->
    Obj = #obj{ptr = 0, bin = B, sz = byte_size(B)},
    case F(Obj) of
        {ok, Xs, #obj{ptr = eof}} -> {ok, Xs, eof};
        {ok, Xs, #obj{sz = Sz, ptr = Ptr}} -> {ok, Xs, Sz-Ptr};
        {error, R} -> {error, R}
    end.

repeat(Min, Max, F, Obj) -> repeat(0, Min, minus1(Max), F, Obj, []).

alternative(_, #obj{ptr = eof}) -> {error, eof};
alternative(Fs, Obj) -> alternative(Fs, Obj, []).

sequence(_, #obj{ptr = eof}) -> {error, eof};
sequence(Fs, Obj) -> sequence(Fs, Obj, []).

'-ALPHA-'(Obj) ->
    F = fun (C) when $A =< C, C =< $Z -> true;
            (C) when $a =< C, C =< $z -> true;
            (_) -> false
        end,
    final(F, Obj).

final(CFUN, Obj) when is_function(CFUN, 1) ->
    case peek_chars(Obj, 1) of
        {[Char], Bin, Sz} ->
            case CFUN(Char) of
                true -> {ok, Bin, bump_ptr(Obj, Sz)};
                false -> {error, {miss, Char}}
            end;
        E -> {error, {miss, E}}
    end;
final(Char, Obj) when is_integer(Char) ->
    case peek_chars(Obj, 1) of
        {[Char], Bin, Sz} -> {ok, Bin, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E}}
    end;
final({C1, C2}, Obj) when is_integer(C1), is_integer(C2) ->
    case peek_chars(Obj, 1) of
        {[C0], Bin, Sz} when C1 =< C0, C0 =< C2 -> {ok, Bin, bump_ptr(Obj, Sz)};
        E -> {error, {miss, E, C1, C2}}
    end;
final(Cs, Obj) when is_list(Cs) ->
    case peek_chars(Obj, 1) of
        {[C], Bin, Sz} ->
            case lists:member(C, Cs) of
                true -> {ok, Bin, bump_ptr(Obj, Sz)};
                false -> {error, {miss, [C]}}
            end;
        E -> {error, {miss, E, Cs}}
    end.

'-DIGIT-'(Obj) ->
    F = fun (C) when $0 =< C, C =< $9 -> true;
            (_) -> false
        end,
    final(F, Obj).

'-WSP-'(Obj) -> final([$\s, $\t], Obj).

'-CRLF-'(Obj) ->
    case final($\n, Obj) of
        {error, Error} -> {error, Error};
        {ok, <<"\n">>, O0} ->
            case final($\r, O0) of
                {ok, <<"\r">>, O} -> {ok, <<"\n\r">>, O};
                {error, _} -> {ok, <<"\n">>, O0}
            end
    end.

'-VCHAR-'(Obj) -> final({$!, $~}, Obj).

'-DQUOTE-'(Obj) -> final($", Obj).

'-BIT-'(Obj) -> final([$0, $1], Obj).

'-HEXDIG-'(Obj) ->
    F = fun (C) when $0 =< C, C =< $9 -> true;
            (C) when $A =< C, C =< $F -> true;
            (_) -> false
        end,
    final(F, Obj).

repeat(N, Mn, Mx, F, Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} when N =:= Mx -> {ok, lists:reverse([X | Xs]), O};
        {ok, X, O} -> repeat(N + 1, Mn, Mx, F, O, [X | Xs]);
        {error, R} when N < Mn -> {error, {too_few, R}};
        {error, _R} -> {ok, lists:reverse(Xs), Obj}
    end.

minus1(inf) -> inf;
minus1(I) -> I - 1.

alternative([], _Obj, Es) -> {error, {fail, Es}};
alternative([F | Fs], Obj, Es) ->
    case F(Obj) of
        {ok, X, O} -> {ok, X, O};
        {error, E} -> alternative(Fs, Obj, [E | Es])
    end.

sequence([], Obj, Xs) -> {ok, lists:reverse(Xs), Obj};
sequence([F | Fs], Obj, Xs) ->
    case F(Obj) of
        {ok, X, O} -> sequence(Fs, O, [X | Xs]);
        {error, R} -> {error, {unexpected, R}}
    end.

bump_ptr(#obj{ptr = Ptr} = Obj, N) -> Obj#obj{ptr = Ptr + N}.

peek_chars(#obj{ptr = Ptr, sz = Sz}, Num) when Sz < Ptr + Num ->
    {error, eof};
peek_chars(#obj{bin = Bin, ptr = Ptr}, 1) ->
    case binary:part(Bin, {Ptr, 1}) of
        <<0:1, C0:7>> = B -> {[C0], B, 1};
        <<6:3, C1:5>> = B1 ->
            case binary:part(Bin, {Ptr + 1, 1}) of
                <<2:2, C2:6>> = B2 -> {[C1 bsl 6 + C2], <<B1/binary, B2/binary>>, 2};
                B2 -> error({unhandled_utf8, B1, B2})
            end;
        B0 -> error({unhandled_utf8, B0})
    end;
peek_chars(#obj{bin = Bin, ptr = Ptr}, Num) ->
    try unicode:characters_to_list(B = binary:part(Bin, {Ptr, Num})) of
        Chars when Num =:= length(Chars) -> {Chars, B, Num};
        {incomplete, H, T} -> error({unhandled_utf8, H, T});
        Chars -> error({unhandled_utf8, Chars})
    catch
        error:badarg -> {error, {miss, eof}}
    end.

numerize(N, Base) when is_list(N) ->
    erlang:list_to_integer(binary_to_list(squeeze(N)), Base).

atomize(A) when is_atom(A) ->
    A;
atomize(X) ->
    binary_to_atom(squeeze(X)).

squeeze(B) when is_binary(B) ->
    B;
squeeze(L) when is_list(L) ->
    binary:list_to_bin(L).
