-module(run).
-export([go/0]).

go() ->
   test:init(),
   try test() of _ -> void
   catch _:_ -> void
   end,
   test:kill().

test() ->
   presence_test:test(),
   schedule_test:test(),
   status_test:test(),
   misc_test:test().
   % signup_test:test()
