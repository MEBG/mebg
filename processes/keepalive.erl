% for launching registered processes that must keep running
-module(keepalive).
-export([launch/4, init/0]).

% initial value of cashbox spawn arguments
initial_cashbox() -> [db:get_transaction_balance()].

% initial value of coop spawn arguments
initial_coop() -> [#{}].

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
init() ->
   inets:start(),
   launch(box,cashbox,loop,fun() -> initial_cashbox() end),
   launch(coop,shop,loop,fun() -> initial_coop() end),
   launch(rcvr,receiver,loop, fun() -> [] end).
