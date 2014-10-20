-module(setup).
-export([start/0]).

start() ->
   register(coop, spawn(shop,loop,[#{}])),
   register(rcvr, spawn(receiver,loop,[])),
   register(box, spawn(cashbox,loop,[0])).