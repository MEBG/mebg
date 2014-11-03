-module(schedule_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").


t500_schedule_add_day_test() ->
   {"111", "You're signed up for Mondays."} = test:send("111", "add monday"),
   {"111", "You're signed up for Mondays and Tuesdays."} = test:send("111", "add tuesday"),
   {"111", "You're signed up for Mondays, Tuesdays and Fridays."} = test:send("111", "add friday").

t502_schedule_add_bad_day_test() ->
   {"111", "You're signed up for Mondays, Tuesdays and Fridays."} = test:send("111", "add bogus things").

t504_schedule_add_none_day_test() ->
   {"111", "You're signed up for Mondays, Tuesdays and Fridays."} = test:send("111", "add").

t510_schedule_remove_bad_day_test() ->
   {"111", "You're signed up for Mondays, Tuesdays and Fridays."} = test:send("111", "remove nothing at all").

t512_schedule_remove_none_day_test() ->
   {"111", "You're signed up for Mondays, Tuesdays and Fridays."} = test:send("111", "remove").

t515_schedule_remove_day_test() ->
   {"111", "You're signed up for Mondays and Fridays."} = test:send("111", "remove tuesday"),
   {"111", "You're signed up for Mondays."} = test:send("111", "remove friday"),
   {"111", "You're not signed up for any shifts."} = test:send("111", "remove monday").


t520_schedule_add_days_test() ->
   {"111", "You're signed up for Mondays and Tuesdays."} = test:send("111", "add monday tuesday").

t530_schedule_remove_days_test() ->
   {"111", "You're not signed up for any shifts."} = test:send("111", "remove monday tuesday").
