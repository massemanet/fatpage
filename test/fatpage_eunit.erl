%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_eunit).

-include_lib("eunit/include/eunit.hrl").

x_test_() ->
  [?_assertEqual(
      {attribute,0,module,demo},
      hd(erl_syntax:revert_forms(fatpage:forms("test/demo.abnf")))),
   ?_assertEqual(
      "test/demo.beam",
      fatpage:beam("test/demo.abnf")),
   ?_assertEqual(
      {ok,[[]],eof},
      demo:string("")),
   ?_assertEqual(
      {ok,[[<<"a">>]],eof},
      demo:string("a")),
   ?_assertEqual(
      [{rule,a,'=',
        {alt,[{seq,[{app,b},{rep,{0,inf},{app,'B'}}]},
              {seq,[{rep,{1,inf},{app,c}},{rep,{0,2},{app,'C'}}]},
              {seq,[{rep,{2,3},{app,d}},{rep,{0,1},{app,'D'}}]}]},
        {construct,[<<"Y">>]}}],
      bootstrap_parse("a=b *B/ 1*c *2C / 2*3d [D];;Y\n"))
  ].

bootstrap_parse(Str) ->
    {ok, Fs, 0} = fatpage_bootstrap_abnf:string(Str),
    refine(Fs).

refine(X) ->
    case X of
        X when is_list(X) -> [refine(E) || E <- X];
        {rule, [N, O, D, C]} -> {rule, N, O, refine(D), C};
        {alt, [A]} -> refine(A);
        {alt, As} -> {alt, refine(As)};
        {seq, [S]} -> refine(S);
        {seq, Ss} -> {seq, refine(Ss)};
        {rep, [], D} -> refine(D);
        {rep, [{1, 1}], D} -> refine(D);
        {rep, [R], D} -> {rep, R, refine(D)};
        _ -> X
    end.
