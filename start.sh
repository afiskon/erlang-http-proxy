#!/bin/sh

erl \
  -name http_proxy@`hostname` \
  -pa ebin deps/*/ebin \
  -config app.config \
  -eval 'lists:foreach(fun(App) -> application:start(App) end, [ ranch, crypto, cowboy, http_proxy ])'
