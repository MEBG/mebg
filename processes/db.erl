-module(db).
% -export([save_member/3,get_role/1,get_expiry/1,get_balance/1]).
-compile(export_all).

open() ->
   sqlite3:open(main).

close() ->
   sqlite3:close(main).

% store new member information in db
save_member(Number, Expiry, Name) ->
   open(),
   sqlite3:write(main, person,[
      {name, Name},
      {phone, Number},
      {expiry, Expiry},
      {role, "member"},
      {balance, 0.0}
   ]),
   close().

get_role(Number) ->
   open(),
   [{columns, _}, {rows, Rows}] = sqlite3:read(main, person, {phone, Number}),
   close(),
   if
      [] =/= Rows ->
         [{_,_,Role,_,_,_}] = Rows,
         %take heed: http://erlang.org/doc/apps/erts/erl_ext_dist.html#utf8_atoms
         binary_to_atom(Role, latin1);
      true ->
         unknown
   end.
   
get_expiry(Number) ->
   open(),
   [{columns, _}, {rows, Rows}] = sqlite3:read(main, person, {phone, Number}),
   close(),
   [{_,_,_,_,Expiry,_}] = Rows,
   Expiry.

get_balance(Number) ->
   open(),
   [{columns, _}, {rows, Rows}] = sqlite3:read(main, person, {phone, Number}),
   close(),
   [{_,_,_,_,_,Balance}] = Rows,
   Balance.


epoch() ->
    now_to_seconds(now()).

now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

store_transaction(Amount, Balance) ->
   open(),
   sqlite3:write(main, transactions, [
      {date, epoch()},
      {amount, Amount},
      {balance, Balance}
   ]),
   close().

% retrieve the last known cashbox balance
get_transaction_balance() ->
   open(),
   [{_,_},{_,[{Balance}]}] = sqlite3:sql_exec(main,
      "SELECT balance FROM transactions ORDER BY date DESC LIMIT 1;"
      ),
   close(),
   Balance.
