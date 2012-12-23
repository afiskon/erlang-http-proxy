-module(http_proxy_SUITE).

-compile(export_all).

suite() ->
    [{timetrap,{seconds,300}}].

init_per_suite(Config) ->
    lists:foreach(
        fun(App) -> application:start(App) end, 
        [ ranch, crypto, cowboy, http_proxy ]
    ),
    ibrowse:start(),
    Config.

end_per_suite(Config) ->
    lists:foreach(
        fun(App) -> application:start(App) end, 
        lists:reverse([ ranch, crypto, cowboy, http_proxy ])
    ),
    ibrowse:stop(),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [
        basic_case
    ].

basic_case() -> 
    [].

basic_case(_Config) ->
    {ok, "200", _Headers, _Data} = ibrowse:send_req(
        "http://ya.ru/", [], get, [], 
        [{proxy_host, "localhost"}, { proxy_port, 8080 }]
    ),
    ok.
