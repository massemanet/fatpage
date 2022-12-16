%% -*- mode: erlang; erlang-indent-level: 4 -*-
-module(fatpage_eunit).

-include_lib("eunit/include/eunit.hrl").

x_test_() ->
  [?_assertEqual(
      {attribute,0,module,demo},
      hd(fatpage:forms("test/demo.abnf"))),
   ?_assertEqual(
      {ok,[[]],eof},
      demo:string("")),
   ?_assertEqual(
      {ok,[[<<"a">>]],eof},
      demo:string("a")),
   ?_assertEqual(
      {ok,[{rule,<<"a">>,
            {alt,[{seq,[{rep,{1,1},<<"b">>},{rep,{0,inf},<<"B">>}]},
                  {seq,[{rep,{1,inf},<<"c">>},{rep,{0,2},<<"C">>}]},
                  {seq,[{rep,{2,3},<<"d">>},
                        {rep,{1,1},
                         {rep,{0,1},{alt,[{seq,[{rep,{1,1},<<"D">>}]}]}}}]}]}}],0},
      fatpage_bootstrap_abnf:string("a=b *B/ 1*c *2C / 2*3d [D]\n"))
  ].
