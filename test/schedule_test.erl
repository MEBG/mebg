-module(schedule_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% Test interactions related to volunteer schedules
%
% add day by unknown
% add day by member
% add and remove day by volunteer
% add day twice
% remove day twice
% add multiple different days at once
% add multiples of same day at once
% add gibberish
% remove gibberish
%

clear_days() ->
   Volunteer = [
      {remove, ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]}, wait
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   test:run(Set).


t100_add_day_unknown_test() ->
   Script = [{add, ["monday"]}, wait],
   Set = [["123", unknown, Script]],
   Expected = [
      {"123", {add, ["monday"]}},
      {{default}, "123"}
   ],
   ?assertEqual(Expected, test:run(Set)).

t102_add_day_member_test() ->
   Script = [{add, ["monday"]}, wait],
   Set = [["112", member, Script]],
   Expected = [
      {"112", {add, ["monday"]}},
      {{default}, "112"}
   ],
   ?assertEqual(Expected, test:run(Set)).

t110_add_remove_day_volunteer_test() ->
   Volunteer = [{add, ["monday"]}, wait, {remove, ["monday"]}, wait],
   Expected = [
      {"111", {add, ["monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t112_add_day_twice_volunteer_test() ->
   Volunteer = [
      {add, ["monday"]}, wait,
      {add, ["monday"]}, wait,
      {remove, ["monday"]}, wait
   ],
   Expected = [
      {"111", {add, ["monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {add, ["monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t114_remove_day_twice_volunteer_test() ->
   Volunteer = [
      {add, ["monday"]}, wait,
      {remove, ["monday"]}, wait,
      {remove, ["monday"]}, wait
   ],
   Expected = [
      {"111", {add, ["monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"},
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t120_remove_day_volunteer_test() ->
   Volunteer = [
      {remove, ["monday"]}, wait
   ],
   Expected = [
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t130_add_multiple_days_volunteer_test() ->
   Volunteer = [
      {add, ["monday", "tuesday"]}, wait,
      {add, ["wednesday", "thursday"]}, wait,
      {add, ["friday", "saturday", "sunday"]}, wait,
      {remove, ["friday", "saturday", "sunday"]}, wait,
      {remove, ["wednesday", "thursday"]}, wait,
      {remove, ["monday", "tuesday"]}, wait
   ],
   Expected = [
      {"111", {add, ["monday", "tuesday"]}},
      {{days, ["Mondays", "Tuesdays"]}, "111"},
      {"111", {add, ["wednesday", "thursday"]}},
      {{days, ["Mondays", "Tuesdays", "Wednesdays", "Thursdays"]}, "111"},
      {"111", {add, ["friday", "saturday", "sunday"]}},
      {{days, ["Mondays", "Tuesdays", "Wednesdays", "Thursdays", "Fridays", "Saturdays", "Sundays"]}, "111"},
      {"111", {remove, ["friday", "saturday", "sunday"]}},
      {{days, ["Mondays", "Tuesdays", "Wednesdays", "Thursdays"]}, "111"},
      {"111", {remove, ["wednesday", "thursday"]}},
      {{days, ["Mondays", "Tuesdays"]}, "111"},
      {"111", {remove, ["monday", "tuesday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t134_add_days_multiply_volunteer_test() ->
   Volunteer = [
      {add, ["monday", "monday"]}, wait,
      {remove, ["monday"]}, wait
   ],
   Expected = [
      {"111", {add, ["monday", "monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {remove, ["monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t134_remove_days_multiply_volunteer_test() ->
   Volunteer = [
      {add, ["monday"]}, wait,
      {remove, ["monday", "monday"]}, wait
   ],
   Expected = [
      {"111", {add, ["monday"]}},
      {{days, ["Mondays"]}, "111"},
      {"111", {remove, ["monday", "monday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t150_mixed_days_test() ->
   Volunteer = [
      {add, ["tuesday"]}, wait,
      {add, ["thursday", "friday"]}, wait,
      {remove, ["wednesday"]}, wait,
      {add, ["wednesday"]}, wait,
      {remove, ["tuesday", "friday"]}, wait,
      {remove, ["thursday", "wednesday"]}, wait
   ],
   Expected = [
      {"111", {add, ["tuesday"]}},
      {{days, ["Tuesdays"]}, "111"},
      {"111", {add, ["thursday", "friday"]}},
      {{days, ["Tuesdays", "Thursdays", "Fridays"]}, "111"},
      {"111", {remove, ["wednesday"]}},
      {{days, ["Tuesdays", "Thursdays", "Fridays"]}, "111"},
      {"111", {add, ["wednesday"]}},
      {{days, ["Tuesdays", "Wednesdays", "Thursdays", "Fridays"]}, "111"},
      {"111", {remove, ["tuesday", "friday"]}},
      {{days, ["Wednesdays", "Thursdays"]}, "111"},
      {"111", {remove, ["thursday", "wednesday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t160_add_nonsense_test() ->
   Volunteer = [
      {add, ["this", "is", "a", "test"]}, wait
   ],
   Expected = [
      {"111", {add, ["this", "is", "a", "test"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).

t162_remove_nonsense_test() ->
   Volunteer = [
      {add, ["thursday", "friday"]}, wait,
      {remove, ["this", "is", "a", "test"]}, wait,
      {remove, ["thursday", "friday"]}, wait
   ],
   Expected = [
      {"111", {add, ["thursday", "friday"]}},
      {{days, ["Thursdays", "Fridays"]}, "111"},
      {"111", {remove, ["this", "is", "a", "test"]}},
      {{days, ["Thursdays", "Fridays"]}, "111"},
      {"111", {remove, ["thursday", "friday"]}},
      {{notsignedup}, "111"}
   ],
   Set = [
      ["111", volunteer, Volunteer]
   ],
   ?assertEqual(Expected, test:run(Set)).
