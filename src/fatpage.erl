%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

-module(fatpage).

-export([init/3, peek/1, backup/1, hit/2, miss/1, callback/1, eof/1]).
-export([choose/2, many/2, many_stash/2, once/2]).

-record(state, {stream, pos=0, cb_fun, cb_acc, stash=[], res=hit, tmp}).

backup(State = #state{pos=Pos}) ->
  State#state{pos=Pos-1}.

peek(State = #state{stream=STREAM0, pos=Pos}) ->
  {STREAM, Char} = STREAM0(STREAM0, Pos),
  {State#state{stream=STREAM, pos=Pos+1}, Char}.

hit(State, El) ->
  State#state{res=hit, tmp=El}.

miss(State) ->
  backup(State#state{res=miss}).

callback(State = #state{res=hit, cb_fun=Fun, cb_acc=Acc, stash=Stash}) ->
  State#state{cb_acc=Fun(Stash, Acc), stash=[]};
callback(State) ->
  State.

eof(#state{cb_fun=Fun, cb_acc=Acc}) ->
  Fun(eof, Acc).

new_frame(State = #state{stash=Stash}) ->
  State#state{stash=[[]|Stash]}.

push_frame(State = #state{stash=[Frame|Stash]}, El) ->
  State#state{stash=[[El|Frame]|Stash]}.

%%-----------------------------------------------------------------------------

choose(State = #state{res=hit}, [Choice|Choices]) ->
  do_choose(Choice(State), Choices);
choose(State, _) ->
  State.

do_choose(State = #state{res=miss}, []) ->
  State;
do_choose(State = #state{res=miss}, [Choice|Choices]) ->
  do_choose(Choice(State#state{res=hit}), Choices);
do_choose(State = #state{res=hit}, _) ->
  State.

many(State0 = #state{res=hit}, Fun) ->
  do_many(Fun(State0), Fun);
many(State = #state{res=miss}, _) ->
  State.

do_many(State = #state{res=hit}, Fun) ->
  do_many(Fun(State), Fun);
do_many(State = #state{res=miss}, _) ->
  State#state{res=hit}.

many_stash(State = #state{res=hit}, Fun) ->
  do_many_stash(Fun(new_frame(State)), Fun);
many_stash(State = #state{res=miss}, _) ->
  State.

do_many_stash(State = #state{res=hit, tmp=Tmp}, Fun) ->
  do_many_stash(Fun(push_frame(State, Tmp)), Fun);
do_many_stash(State = #state{res=miss}, _) ->
  State#state{res=hit}.

once(State = #state{res=hit}, Fun) ->
  Fun(State);
once(State = #state{res=miss}, _) ->
  State.

%%-----------------------------------------------------------------------------

init(Filename, Fun, Acc) ->
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
