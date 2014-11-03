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

   sqlite3:create_table(main, schedule_recurring, [
      {id, integer, [primary_key]},
      {volunteer, integer},
      {day, integer}
   ]),

   sqlite3:create_table(main, schedule_event, [
      {id, integer, [primary_key]},
      {volunteer, integer},
      {present, boolean},
      {date, integer}
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
   ]),

   sqlite3:create_table(main, presence, [
      {id, integer, [primary_key]},
      {present, boolean},
      {timestamp, integer},
      {person_id, integer}
   ]).

init() ->
   sqlite3:write(main, person, [{name, "Bot01"}, {phone, "111"}, {role, "volunteer"}, {balance, 0.0}]),
   sqlite3:write(main, person, [{name, "Bot02"}, {phone, "222"}, {role, "volunteer"}, {balance, 0.0}]).
