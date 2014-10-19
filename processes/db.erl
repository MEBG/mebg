-module(db).
-export([save_member/3,get_role/1,get_expiry/1,get_balance/1]).

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
   Number.

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