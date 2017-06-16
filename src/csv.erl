%% -*- mode: erlang; erlang-indent-level: 2 -*-
%% @doc
%% @end

%% ABNF as per RFC4180
%%
%% file        = record *eol-record [EOL]
%% eol-record  = EOL record
%% record      = field *comma-field                 : pop().
%% comma-field = COMMA field
%% field       = escaped / non-escaped
%% escaped     = DQUOTE *qchar DQUOTE               : push(2).
%% non-escaped = *TEXTDATA                          : push(1).
%% qchar       = DQCHAR / SQCHAR
%% DQCHAR      = %x22 %x22                          : sub(34).
%% SQCHAR      = %x0A / %x0D / %x20-21 / %x23-7E
%% COMMA       = %x2C
%% DQUOTE      = %x22
%% EOL         = %x0D %x0A / %x0D / %x0A
%% TEXTDATA    = %x20-21 / %x23-2B / %x2D-7E

-module(csv).
-include("fatpage.hrl").

%%-----------------------------------------------------------------------------
%% CSV rules
%% generated from the ABNF

s_init(S0) ->
  s_file(S0).

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
  S1 = close(many(open(S), fun s_qchar/1)),
  once(S1, fun l_DQUOTE/1).

s_non_escaped(S0) ->
  close(many(open(S0), fun l_TEXTDATA/1)).

s_qchar(S0) ->
  Choices = [fun l_SQCHAR/1, fun l_DQCHAR/1],
  choose(S0, Choices).

%%-----------------------------------------------------------------------------
%% CSV literals
%% generated from the ABNF

l_DQCHAR(State0) ->
  {State1, Char0} = peek(State0),
  if
    (Char0 =:= $") ->
      {State2, Char1} = peek(State1),
      if
        Char1 =:= $" -> hit(State2, 34);
        true -> miss(backup(State2))
      end;
    true ->
      miss(State1)
  end.

l_SQCHAR(State0) ->
  {State1, Char0} = peek(State0),
  if
    (Char0 =:= $\n) orelse
    (Char0 =:= $\r) orelse
    ((16#20 =< Char0) andalso (Char0 =< 16#21)) orelse
    ((16#23 =< Char0) andalso (Char0 =< 16#7E)) ->
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
