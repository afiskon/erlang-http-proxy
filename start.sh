#!/bin/sh

export ERL_CRASH_DUMP_SECONDS=1

erl \
  -name http_proxy@`hostname` \
  -pa ebin deps/*/ebin \
  -config app.config \
  -eval 'lists:foreach(fun(App) -> ok = application:start(App) end, [ ranch, crypto, cowboy, http_proxy ])'
