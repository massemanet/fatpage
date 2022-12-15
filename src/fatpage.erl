-module(fatpage).

%% API
-export(
   [parse/1,
    unroll/1,
    left_recursive/1,
    forms/1,
    pp/1,
    erl/1,
    display/1,
    beam/1
   ]).

%% rebar3 provider callbacks
-export(
   [command_line_args/1,
    format_error/1
   ]).

%% callbacks from rebar3 plugin
command_line_args(As) ->
    case proplists:get_value(task, As, undefined) of
        undefined -> error({no_target, As});
        File -> opts(File)
    end.

format_error(Error) -> flat("~w", [Error]).

opts(File) ->
    maps:from_list([{in, File}, {out, out_file(File, beam)}]).

%% API

beam(Filename) when not is_map(Filename) ->
    beam(opts(Filename));
beam(Opts) when is_map(Opts) ->
    #{in := File, out := OutFile} = Opts,
    {ok, Mod, Beam} = compile:forms(forms(File), [debug_info]),
    file:write_file(OutFile, Beam),
    code:load_binary(Mod, OutFile, Beam),
    OutFile.

display(Filename) ->
    io:fwrite("~s", [pp(Filename)]).

erl(Filename) ->
    OutFile = out_file(Filename, erl),
    Erl = pp(Filename),
    write_file(OutFile, Erl),
    OutFile.

pp(Filename) ->
    fatpage_gen:pp(forms(Filename), Filename).

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
