-module(http_proxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ibrowse:start(),
    lager:start(),
    Dispatch = [
            {'_', [ { '_', toppage_handler, [] } ] }
        ],
    {ok, Port} = application:get_env(port),
    {ok, Timeout} = application:get_env(timeout),
    {ok, Workers} = application:get_env(workers),
    {ok, _} = cowboy:start_http(http, Workers,
        [{port, Port}],
        [{dispatch, Dispatch},{timeout, Timeout}]
    ),
    http_proxy_sup:start_link().

stop(_State) ->
    ok.
