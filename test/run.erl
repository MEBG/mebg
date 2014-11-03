-module(run).
-export([go/0]).

go() -> 
   test:init(),
   presence_test:test(),
   schedule_test:test().   