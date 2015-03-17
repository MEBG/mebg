-module(misc_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%
% Test for miscellaneous features that do not fall into any larger category
%
%
% ** locked out **
%
% - recognizable strings: ["door","locked"]
%
% contexts:
% 1 - no one signed in
%
% send a response: (number, "Sorry, no one is here right now..")
%
%
% 2 - one or more volunteers signed in
%
% send two responses:
%  (number, "Not again! Hang tight, someone will be down in a minute.")
%  (volunteer, "Someone is locked out downstairs!")
%
% if more than one volunteer is signed in,
% all volunteers receive the response
%

% no one signed in
t100_locked_empty_pass_test() ->
   Script = [{send, door}, {wait, {door_closed}}],
   Expected = [{"123", door}, {{door_closed}, "123"}],
   Roster = [["123", unknown, Script]],
   test:run(Roster, Expected, true).

signin_test() ->
   Script = [
      {send, in},
      {wait, {hello}},
      {send, out},
      {wait, {bye}}
   ],
   Roster = [["111", volunteer, Script]],
   Expected = [
      {"111", in},
      {{hello}, "111"},
      {"111", out},
      {{bye}, "111"}
   ],
   test:run(Roster, Expected, true).


% t101_locked_empty_fail_test() ->
%    Script = [{send, door}, {wait, {door_closed}}],
%    spawn(actor, init, [{"123", unknown}, Script, self()]),
%    test:validate(100).

% t110_locked_one_volunteer_pass_test() ->
%    Volunteer = [{send, door}, {wait, {door_open}}],
   % Volunteer = [{send, {in, null}}, {wait, {hello}}, {send, {out, null}}, {wait, {bye}}],
   % Volunteer = [{send, {open, null}}, {wait, {hello}}, {wait, {door_open}}],
   % spawn(actor, init, [{"111", volunteer}, Volunteer]).
   % timer:sleep(100),
   % Visitor = [{send, door}, {wait, {door_wait}}],
   % spawn(actor, init, [{"123", unknown}, Visitor]),
   % test:validate(300).


% one volunteer signed in
% t200_locked_one_test() ->
%     Member = "+123",
%     Volunteer = "+111",
%     rcvr ! {Volunteer, "in"},
%     timer:sleep(100),
%     E = spawn(test, expected,
% 	      [{Volunteer, "Someone is locked out downstairs!"},
% 	       {Member,
% 		"Not again! Hang tight, someone will "
% 		"be down in a minute."}]),
%     E ! q,
%     rcvr ! {Member, "locked"},
%     timer:sleep(100),
%     listener ! E,
%     listener ! E,
%     rcvr ! {Volunteer, "out"}.
