#!/bin/bash
echo Presence tests
erl -pa ../src -noshell -eval 'test:init(), presence_test:test(), test:kill(), init:stop()'
echo Schedule tests
erl -pa ../src -noshell -eval 'test:init(), schedule_test:test(), test:kill(), init:stop()'
echo Status tests
erl -pa ../src -noshell -eval 'test:init(), status_test:test(), test:kill(), init:stop()'
echo Misc tests
erl -pa ../src -noshell -eval 'test:init(), misc_test:test(), test:kill(), init:stop().'