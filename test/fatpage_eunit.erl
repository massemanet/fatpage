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
      demo:string("a"))
  ].
