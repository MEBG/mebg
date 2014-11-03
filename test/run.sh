#!/bin/bash
erl -pa ../src -noshell -eval 'test:init(), presence_test:test(), init:stop()'
erl -pa ../src -noshell -eval 'test:init(), schedule_test:test(), init:stop()'