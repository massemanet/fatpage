-module(fatpage).

-export(
   [parse/1,
    unroll/1,
    left_recursive/1,
    forms/1,
    pp/1,
    erl/1,
    display/1,
    beam/1]).

beam(Filename) ->
    OutFile = out_file(Filename, beam),
    {ok, _Mod, Beam} = compile:forms(forms(Filename)),
    file:write_file(OutFile, Beam),
    OutFile.

display(Filename) ->
    io:fwrite("~s", [pp(Filename)]).

erl(Filename) ->
    OutFile = out_file(Filename, erl),
    Erl = pp(Filename),
    write_file(OutFile, Erl),
    OutFile.

pp(Filename) ->
    fatpage_gen:pp(forms(Filename)).

forms(Filename) ->
    fatpage_gen:forms(mod(Filename), unroll(Filename)).

left_recursive(Filename) ->
    fatpage_gen:left_recursive(unroll(Filename)).

unroll(Filename) ->
    fatpage_gen:unroll(parse(Filename)).

parse(Bin) when is_binary(Bin) ->
    parse(binary_to_list(Bin));
parse(String) when is_list(String) ->
    fatpage_gen:parse(subject(String)).

subject(String) ->
    case filelib:is_regular(String) of
        true -> binary_to_list(lift(ok, file:read_file(String)));
        false -> String
    end.

lift(Tag, {Tag, V}) -> V;
lift(Tag, {T, _}) -> error({tag_mismatch, Tag, T}).

mod(Filename) ->
    filename:basename(Filename, ".abnf").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% generate beam and write .beam file

out_file(Filename, Ext) ->
    Dir = filename:dirname(Filename),
    Base = filename:basename(Filename, ".abnf"),
    flat("~s/~s.~s", [Dir, Base, Ext]).

flat(F, As) ->
    lists:flatten(io_lib:format(F, As)).

write_file(OutFile, String) ->
    {ok, FD} = file:open(OutFile, [write]),
    io:fwrite(FD, "~s", [String]),
    file:close(FD).
