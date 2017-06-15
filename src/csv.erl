%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

%% ABNF as per RFC4180
%%
%%    file = record *(eol_record) [EOL]
%%    eol_record = EOL record
%%    record = field *(comma_field)
%%    comma_field = COMMA field
%%    field = (escaped / non-escaped)
%%    escaped = DQUOTE *QCHAR DQUOTE
%%    non-escaped = *TEXTDATA
%%    QCHAR = (%x20-21 / %x23-7E / %x0D / %x0A / 2DQUOTE)
%%    COMMA = %x2C
%%    DQUOTE =  %x22
%%    EOL = ( %x0D / %x0A / %x0D %x0A )
%%    TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E

-module(csv).

-export([fold/3, parse/1]).

-record(state, {stream, pos=0, cb_fun, cb_acc, stash, open=false}).

parse(Filename) ->
  fold(Filename, fun parser/2, []).

fold(Filename, Fun , Acc) ->
  eof(s_file(mk_state(Filename, Fun, Acc))).

parser([[]], Acc) -> Acc;                 % empty line
parser(eof, Acc) -> lists:reverse(Acc);
parser(Rec, Acc) -> [Rec|Acc].

%%-----------------------------------------------------------------------------

s_file(S0) ->
  S1 = once(S0, fun s_record/1),
  many(S1, fun s_eol_record/1).

s_record(S0) ->
  S1 = once(S0, fun s_field/1),
  S2 = many(S1, fun s_comma_field/1),
  pop(S2).

s_eol_record(S0) ->
  S1 = once(S0, fun l_EOL/1),
  once(S1, fun s_record/1).

s_comma_field(S0) ->
  S1 = once(S0, fun l_COMMA/1),
  once(S1, fun s_field/1).

s_field(S0) ->
  Choices = [fun s_escaped/1, fun s_non_escaped/1],
  choose(S0, Choices).

s_escaped(S0) ->
  S = once(S0, fun l_DQUOTE/1),
  S1 = close(many(open(S), fun l_QCHAR/1)),
  once(S1, fun l_DQUOTE/1).

s_non_escaped(S0) ->
  close(many(open(S0), fun l_TEXTDATA/1)).

%%-----------------------------------------------------------------------------

l_QCHAR(State0) ->
  {State1, Char0} = peek(State0),
  if
    (Char0 =:= $") ->
      {State2, Char1} = peek(State1),
      if
        Char1 =:= $" -> hit(State2, Char1);
        true -> miss(backup(State2))
      end;
    (Char0 =:= $\n) orelse
    (Char0 =:= $\r) orelse
    ((16#20 =< Char0) andalso (Char0 =< 16#7E)) ->
      hit(State1, Char0);
    true ->
      miss(State1)
  end.

l_COMMA(State0) ->
  {State1, Char} = peek(State0),
  if
    Char =:= $, -> hit(State1, $,);
    true -> miss(State1)
  end.

l_DQUOTE(State0) ->
  {State1, Char} =  peek(State0),
  if
    Char =:= $" -> hit(State1, $");
    true -> miss(State1)
  end.

l_EOL(State0) ->
  {State1, Char0} = peek(State0),
  if
    Char0 =:= 16#0D ->
      {State2, Char1} = peek(State1),
      if
        Char1 =:= 16#0A -> hit(State2, eol);
        true -> hit(backup(State2), eol)
      end;
    Char0 =:= 16#0A ->
      hit(State1, eol);
    true ->
      miss(State1)
  end.

l_TEXTDATA(State0) ->
  {State1, Char} = peek(State0),
  if
    (16#20 =< Char andalso Char =< 16#21) orelse
    (16#23 =< Char andalso Char =< 16#2B) orelse
    (16#2D =< Char andalso Char =< 16#7E) ->
      hit(State1, Char);
    true ->
      miss(State1)
  end.

%%-----------------------------------------------------------------------------

hit(State, El) ->
  case State#state.open of
    false -> State;
    true ->
      [Stash|Stashs] = State#state.stash,
      State#state{stash = [[El|Stash]|Stashs]}
  end.

miss(State) ->
  throw(backup(State)).

pop(State = #state{cb_fun=Fun, cb_acc=Acc, stash=Stash}) ->
  State#state{cb_acc=Fun(lists:reverse(Stash), Acc), stash=undefined}.

open(State) ->
  case State#state.stash of
    undefined -> State#state{open=true, stash=[[]]};
    Stash -> State#state{open=true, stash=[[]|Stash]}
  end.

close(State = #state{stash=[Stash|Stashs]}) ->
  State#state{open=false, stash=[lists:reverse(Stash)|Stashs]}.

eof(#state{cb_fun=Fun, cb_acc=Acc}) ->
  Fun(eof, Acc).

%%-----------------------------------------------------------------------------

choose(State, []) ->
  throw(State);
choose(State, [Choice|Choices]) ->
  try Choice(State)
  catch State1 -> choose(State1, Choices)
  end.

many(State, Fun) ->
  try many(Fun(State), Fun)
  catch State1 -> State1
  end.

once(State, Fun) ->
  Fun(State).

%%-----------------------------------------------------------------------------

peek(State = #state{stream=STREAM0, pos=Pos}) ->
  {STREAM, Char} = STREAM0(STREAM0, Pos),
  {State#state{stream=STREAM, pos=Pos+1}, Char}.

backup(State = #state{pos=Pos}) ->
  State#state{pos=Pos-1}.

mk_state(Filename, Fun , Acc) ->
  #state{stream=mk_stream(Filename), cb_fun=Fun, cb_acc=Acc}.

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
