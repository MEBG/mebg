% for launching registered processes that must keep running
-module(keepalive).
-export([launch/4, launch/0]).

% initial value of cashbox spawn arguments
default_cashbox() -> [db:get_transaction_balance()].

% initial value of coop spawn arguments
default_coop() -> [#{}].

launch(Name, Module, Fun, Args) ->
   Pid = spawn(Module,Fun,Args()),
   register(Name,Pid),
   spawn(fun() ->
      Ref = monitor(process, Pid),
      receive
         {'DOWN', Ref, process, Pid, Why} ->
            Msg = lists:concat(["Process '",Name, "' crashed."]),
            sender:send(secrets:admin_number(), Msg),
            io:format("~p (~p) died: ~p~n", [Pid,Name,Why]),
            io:format("restarting ~p~n", [Name]),
            launch(Name,Module,Fun,Args)
      end
   end).

% launch system
launch() ->
   inets:start(),
   launch(box,cashbox,loop,fun() -> default_cashbox() end),
   launch(coop,shop,loop,fun() -> default_coop() end),
   launch(rcvr,receiver,loop, fun() -> [] end).

