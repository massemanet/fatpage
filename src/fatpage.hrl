%%-----------------------------------------------------------------------------
%% this is the fatpage driver. gets included in the code that's generated from
%% the ABNF spec by 'fatpage_gen'.

-export([fold/3, parse/1]).

-record(state, {stream, pos=0, cb_fun, cb_acc, stash, open=false}).

%%-----------------------------------------------------------------------------
%% API

parse(Filename) ->
  fold(fun default_cb/2, [], Filename).

fold(Fun, Acc, Filename) ->
  eof(s_init(#state{stream=mk_stream(Filename), cb_fun=Fun, cb_acc=Acc})).

%%-----------------------------------------------------------------------------
%% default callback

default_cb([[]], Acc) -> Acc;                 % empty line
default_cb(eof, Acc) -> lists:reverse(Acc);
default_cb(Rec, Acc) -> [Rec|Acc].


%%-----------------------------------------------------------------------------
%% state operations

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

peek(State = #state{stream=STREAM0, pos=Pos}) ->
  {STREAM, Char} = STREAM0(STREAM0, Pos),
  {State#state{stream=STREAM, pos=Pos+1}, Char}.

backup(State = #state{pos=Pos}) ->
  State#state{pos=Pos-1}.

%%-----------------------------------------------------------------------------
%% ABNF operations

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
%% stream handling

mk_stream(Filename) ->
  case file:open(Filename, [read, raw, binary, compressed]) of
    {ok, FD} -> mk_streamf(mk_reader(FD));
    {error, R} -> error({open_error, R, Filename})
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

mk_streamf(READER) ->
  mk_streamf(READER, 0, READER(read), READER(read)).

mk_streamf(READER, Offset, {Size0, Chunk0}, {Size1, Chunk1}) ->
  fun(This, Pos) ->
      if
        Pos < Offset -> error({read_out_of_range, #{pos=>Pos, offset=>Offset}});
        Pos < Offset+Size0 -> {This, char(Pos-Offset, Chunk0)};
        Pos < Offset+Size0+Size1 -> {This, char(Pos-Offset-Size0, Chunk1)};
        true ->
          case Chunk1 of
            eof ->
              READER(close),
              {This, eof};
            _ ->
              R = READER(read),
              STREAM = mk_streamf(READER, Offset+Size0, {Size1, Chunk1}, R),
              STREAM(STREAM, Pos)
          end
      end
  end.

char(Pos, Chunk) ->
  <<_:Pos/binary, Char, _/binary>> = Chunk,
  Char.
