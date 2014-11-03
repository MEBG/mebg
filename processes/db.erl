-module(db).
% -export([save_member/3,get_role/1,get_expiry/1,get_balance/1]).
-compile(export_all).

% datetime conversion
epoch() ->
    now_to_seconds(now()).
now_to_seconds({Mega, Sec, _}) ->
    (Mega * 1000000) + Sec.

open() ->
   sqlite3:open(main).

close() ->
   sqlite3:close(main).

get_rows(Table, Filter) ->
   open(),
   [{_,_}, {_,Rows}] = sqlite3:read(main, Table, Filter),
   close(),
   Rows.

%
% person
%
%

% assumes sqlite3 connection is open
get_person_id(Number) ->
   [{_,_},{_,[{PersonId}]}] = sqlite3:sql_exec(main,
      lists:concat(["SELECT id FROM person WHERE phone = \"",
         Number, "\";"])
   ),
   PersonId.

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

% retrieve info of matching person from storage
% or create a new unknown person if not found
get_person(Number) ->
   get_person_from_rows(Number,
      get_rows(person, {phone, Number})).

get_person_from_rows(Number, Rows) when Rows == [] ->
   {void, Number, unknown, void, void, void};
get_person_from_rows(Number, Rows) ->
   [{PersonId, _, Role, Name, Expiry, Balance}] = Rows,
   {
      PersonId,
      Number,
      binary_to_atom(Role, latin1),
      binary_to_list(Name),
      Expiry,
      Balance
   }.

get_name(Number) ->
   {_,_,_,Name,_,_} = get_person(Number),
   Name.

get_role(Number) ->
   {_,_,Role,_,_,_} = get_person(Number),
   Role.

get_expiry(Number) ->
   {_,_,_,_,Expiry,_} = get_person(Number),
   Expiry.

get_balance(Number) ->
   {_,_,_,_,_,Balance} = get_person(Number),
   Balance.


%
% transactions
%

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

% store "arrive" and "depart" instances
set_presence(Number, Present) ->
   open(),
   sqlite3:write(main, presence, [
      {present, Present},
      {timestamp, epoch()},
      {person_id, get_person_id(Number)}
      ]),
   close().


%
% schedule
%

remove_day(Number, Day) ->
   open(),
   Query = lists:concat([
         "DELETE FROM schedule_recurring WHERE volunteer = ",
         get_person_id(Number), " AND day = '", Day, "';"
   ]),
   sqlite3:sql_exec(main,Query),
   close().

add_day(Number, Day) ->
   remove_day(Number, Day), % avoid duplicates
   open(),
   Query = lists:concat([
         "INSERT INTO schedule_recurring (volunteer, day) ",
         "VALUES (", get_person_id(Number), ", ", Day, ");"
   ]),
   {rowid, _} = sqlite3:sql_exec(main,Query),
   close().

get_days(Number) ->
   open(),
   Query = lists:concat([
      "SELECT Day FROM schedule_recurring WHERE volunteer = ",
      get_person_id(Number), " ORDER BY Day;"
   ]),
   [{_,_},{rows,Rows}] = sqlite3:sql_exec(main,Query),
   close(),
   Days = [{1,"Mondays"},{2,"Tuesdays"},{3,"Wednesdays"},{4,"Thursdays"},{5,"Fridays"},{6,"Saturdays"},{7,"Sundays"}],
   [ N || {_,N} <- [lists:keyfind(X,1,Days) || {X} <- Rows ] ].

get_schedule_today() ->
   open(),
   {Date,_} = erlang:localtime(),
   Day = calendar:day_of_the_week(Date),
   Query = lists:concat([
      "SELECT p.name FROM person p
      inner join schedule_recurring sr on p.id = sr.volunteer
      WHERE day = ", Day, ";"
   ]),
   [{_,_},{rows,Rows}] = sqlite3:sql_exec(main,Query),   
   close(),
   greetings:concatenate([binary_to_list(R) || {R} <- Rows]).