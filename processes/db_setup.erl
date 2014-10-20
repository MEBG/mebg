% initialize default database
-module(db_setup).
-export([go/0]).

go() ->
   file:delete("main.db"),
   sqlite3:open(main),
   tables(),
   init(),
   sqlite3:close(main).

tables() ->
   sqlite3:create_table(main, person, [
      {id, integer, [primary_key]},
      {phone, text},
      {role, text},
      {name, text},
      {expiry, integer},
      {balance, float}
   ]),

   sqlite3:create_table(main, transactions, [
      {id, integer, [primary_key]},
      {date, integer},
      {amount, float},
      {balance, float},
      {volunteer, text},
      {member, text}
   ]),

   sqlite3:create_table(main, inventory, [
      {id, integer, [primary_key]},
      {sku, text},
      {count, integer}
   ]).

init() ->
   sqlite3:write(main, person, [{name, "Bot01"}, {phone, "111"}, {role, "volunteer"}, {balance, 0.0}]),
   sqlite3:write(main, person, [{name, "Bot02"}, {phone, "222"}, {role, "volunteer"}, {balance, 0.0}]).
