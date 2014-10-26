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
   % awkward two-step
   [{_,_},{_,[{PersonId}]}] = sqlite3:sql_exec(main,
      lists:concat(["SELECT id FROM person WHERE phone = \"",
         Number, "\";"])
   ),
   sqlite3:write(main, presence, [
      {present, Present},
      {timestamp, epoch()},
      {person_id, PersonId}
      ]),
   close().
