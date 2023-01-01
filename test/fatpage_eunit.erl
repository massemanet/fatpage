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
      {ok,[{rule,a,
            {alt,[{seq,[{app,b},{rep,{0,inf},{app,'B'}}]},
                  {seq,[{rep,{1,inf},{app,c}},{rep,{0,2},{app,'C'}}]},
                  {seq,[{rep,{2,3},{app,d}},{rep,{0,1},{app,'D'}}]}]},
            <<"Y">>}],
       0},
      fatpage_bootstrap_abnf:string("a=b *B/ 1*c *2C / 2*3d [D]\n"))
  ].
