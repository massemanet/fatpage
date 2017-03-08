%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end
%% file ::= line* 'eof'
%% line ::=  '\n' | !'eof' string_list
%% string_list ::= string cstring* '\n'
%% string ::= ws* [field_ws]
%% field_ws ::= field ws*
%% cstring ::= ',' string
%% ws ::= ' ' | '\t'
%% field ::= '"' q | !uq (!uq)*
%% q ::= iq '"'
%% uq ::= '\n' | '\t' | ' ' | ',' | '"'
%% iq ::= iqs ddq_iqs*
%% ddq_iqs ::= '"' '"' iqs
%% iqs ::= (!'"')*

-module(csv).

-export([fold/3]).

fold(Filename, Fun , Acc) ->
  top({mk_stream(Filename), 0, Fun, Acc}, fun s_file/1).

%%-----------------------------------------------------------------------------

s_file(State0) ->
  State = many(State0, fun s_line/1),
  once(State, fun eof/1).

s_line(State0) ->
  case peek(State0) of
    {State, $\n} -> bump(State, $\n);
    {State, eof} -> throw(State);
    {State, _} -> once(State, fun s_string_list/1)
  end.

s_string_list(State0) ->
  State = once(State0, fun s_string/1),
  State1 = many(State, fun s_cstring/1),
  once(State1, fun l_nl/1).

s_cstring(State0) ->
  State = once(State0, fun l_comma/1),
  once(State, fun s_string/1).

s_string(State0) ->
  State = many(State0, fun l_ws/1),
  option(State, fun s_field_ws/1).

s_field_ws(State0) ->
  State = once(State0, fun s_field/1),
  many(State, fun l_ws/1).

s_field(State0) ->
  case peek(State0) of
    {State, $"} -> once(bump(State, $"), fun s_q/1);
    {State, $\n} -> throw(State);
    {State, $\t} -> throw(State);
    {State, $ } -> throw(State);
    {State, $,} -> throw(State);
    {State, _} -> many(State, fun l_not_uq/1)
  end.

s_q(State0) ->
  State = once(State0, fun s_iq/1),
  once(State, fun l_dq/1).

s_iq(State0) ->
  State = once(State0, fun s_iqs/1),
  many(State, fun s_ddq_iqs/1).

s_ddq_iqs(State0) ->
  State = once(State0, fun l_dq/1),
  State1 = once(State, fun l_dq/1),
  once(State1, fun s_iqs/1).

s_iqs(State) ->
  many(State, fun l_not_dq/1).

%%-----------------------------------------------------------------------------

l_nl(State) ->
  literal(State, $\n).

l_comma(State) ->
  literal(State, $,).

l_dq(State) ->
  literal(State, $").

l_ws(State0) ->
  case peek(State0) of
    {State, $\t} -> bump(State, $\t);
    {State, $ } -> bump(State, $ );
    {State, _} -> throw(State)
  end.

l_not_dq(State0) ->
  case peek(State0) of
    {State, $"} -> throw(State);
    {State, Char} -> bump(State, Char)
  end.

l_not_uq(State0) ->
  case peek(State0) of
    {State, $\n} ->  throw(State);
    {State, $\t} -> throw(State);
    {State, $ } -> throw(State);
    {State, $,} -> throw(State);
    {State, $"} -> throw(State);
    {State, Char} -> bump(State, Char)
  end.

literal(State0, Char) ->
  case peek(State0) of
    {State, Char} -> bump(State, Char);
    {State, _} -> throw(State)
  end.

%%-----------------------------------------------------------------------------

bump({STREAM, Pos, Fun, Acc}, Char) ->
  {STREAM, Pos+1, Fun, Fun(Char, Acc)}.

peek({STREAM0, Pos, Fun, Acc}) ->
  {STREAM, Char0} = STREAM0(STREAM0, Pos),
  {{STREAM, Pos, Fun, Acc}, Char0}.

eof(State0) ->
  case peek(State0) of
    {State, eof} -> State;
    {State, _} -> throw(State)
  end.
%%-----------------------------------------------------------------------------
option(State0, Fun) ->
  try Fun(State0)
  catch State -> State
  end.

many(State0, Fun) ->
  try many(Fun(State0), Fun)
  catch State -> State
  end.

once(State0, Fun) ->
  try Fun(State0)
  catch State -> throw(State)
  end.

top(State0, Fun) ->
  try Fun(State0)
  catch
    State ->
      exit(#{reason=>syntax_error,
             pos=>element(2, State),
             char=>[element(2, peek(State))],
             function=>proplists:get_value(name, erlang:fun_info(Fun))})
  end.

%%-----------------------------------------------------------------------------
mk_stream(Filename) ->
  case file:open(Filename, [read, raw, binary, compressed]) of
    {ok, FD} ->
      READER = mk_reader(FD),
      mk_streamf(READER, READER(read));
    {error, R} ->
      error({open_error, R, Filename})
  end.

mk_reader(FD) ->
  fun(Cmd) ->
      case Cmd of
        close ->
          file:close(FD);
        read ->
          case file:read(FD, 1048576) of
            {ok, Chunk} ->
              {size(Chunk), Chunk};
            eof ->
              {0, eof};
            {error, R} ->
              error({read_error, R})
          end
      end
  end.

mk_streamf(READER, {OSize, OChunk}) ->
  {Size, Chunk} = READER(read),
  fun(This, Pos) ->
      try <<_:Pos/binary, Char, _/binary>> = OChunk,
           {This, Char}
      catch _:_ ->
          case Pos == OSize andalso Size == 0 of
            true -> READER(close), {This, eof};
            false -> error(#{pos=>Pos, osize=>OSize, chunk=>Chunk})
          end
      end
  end.
