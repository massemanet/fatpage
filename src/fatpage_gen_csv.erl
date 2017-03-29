%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
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
%% @end

-module(fatpage_gen_csv).

-export([root/1]).

-record(state, {stream, pos=0, cb_fun, cb_acc, stash=[], res=hit, tmp}).

-define(ONCE(S,F),
        case S#state.res of
          hit -> F(S);
          miss -> S
        end).
-define(MANY(S,F),
        case S#state.res of
          hit -> do_many(F(S), fun F/1);
          miss -> S
        end).

do_many(State = #state{res=hit}, Fun) ->
  do_many(Fun(State), Fun);
do_many(State = #state{res=miss}, _) ->
  State#state{res=hit}.

root(S) ->
  s_file(S).

s_file(S0) ->
  S1 = ?ONCE(S0, s_record),
  ?MANY(S1, s_eol_record).

s_record(S0) ->
  S1 = fatpage:once(S0, fun s_field/1),
  fatpage:callback(fatpage:many(S1, fun s_comma_field/1)).

s_eol_record(S0) ->
  S1 = fatpage:once(S0, fun l_EOL/1),
  fatpage:once(S1, fun s_record/1).

s_comma_field(S0) ->
  S1 = fatpage:once(S0, fun l_COMMA/1),
  fatpage:once(S1, fun s_field/1).

s_field(S0) ->
  Choices = [fun s_escaped/1, fun s_non_escaped/1],
  fatpage:choose(S0, Choices).

s_escaped(S0) ->
  S = fatpage:once(S0, fun l_DQUOTE/1),
  S1 = fatpage:many_stash(S, fun l_QCHAR/1),
  fatpage:once(S1, fun l_DQUOTE/1).

s_non_escaped(S0) ->
  fatpage:many_stash(S0, fun l_TEXTDATA/1).

%%-----------------------------------------------------------------------------

l_QCHAR(State0) ->
  {State1, Char0} = fatpage:peek(State0),
  if
    (Char0 =:= $") ->
      {State2, Char1} = fatpage:peek(State1),
      if
        Char1 =:= $" -> fatpage:hit(State2, Char1);
        true -> fatpage:miss(fatpage:backup(State2))
      end;
    (Char0 =:= $\n) orelse
    (Char0 =:= $\r) orelse
    ((16#20 =< Char0) andalso (Char0 =< 16#7E)) ->
      fatpage:hit(State1, Char0);
    true ->
      fatpage:miss(State1)
  end.

l_COMMA(State0) ->
  {State1, Char} = fatpage:peek(State0),
  if
    Char =:= $, -> fatpage:hit(State1, $,);
    true -> fatpage:miss(State1)
  end.

l_DQUOTE(State0) ->
  {State1, Char} =  fatpage:peek(State0),
  if
    Char =:= $" -> fatpage:hit(State1, $");
    true -> fatpage:miss(State1)
  end.

l_EOL(State0) ->
  {State1, Char0} = fatpage:peek(State0),
  if
    Char0 =:= 16#0D ->
      {State2, Char1} = fatpage:peek(State1),
      if
        Char1 =:= 16#0A -> fatpage:hit(State2, eol);
        true -> fatpage:hit(fatpage:backup(State2), eol)
      end;
    Char0 =:= 16#0A ->
      fatpage:hit(State1, eol);
    true ->
      fatpage:miss(State1)
  end.

l_TEXTDATA(State0) ->
  {State1, Char} = fatpage:peek(State0),
  if
    (16#20 =< Char andalso Char =< 16#21) orelse
    (16#23 =< Char andalso Char =< 16#2B) orelse
    (16#2D =< Char andalso Char =< 16#7E) ->
      fatpage:hit(State1, Char);
    true ->
      fatpage:miss(State1)
  end.
