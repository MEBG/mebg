#!/bin/bash
erl -pa ../src -noshell -eval 'test:init(), presence_test:test(), test:kill(), init:stop()'
erl -pa ../src -noshell -eval 'test:init(), schedule_test:test(), test:kill(), init:stop()'
erl -pa ../src -noshell -eval 'test:init(), status_test:test(), test:kill(), init:stop()'
erl -pa ../src -noshell -eval 'test:init(), misc_test:test(), test:kill(), init:stop().'