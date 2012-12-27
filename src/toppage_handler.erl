-module(toppage_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/2]).

-type request() :: cowboy_req:req().
-type headers() :: cowboy_http:headers().
-type processor() :: fun((binary()) -> binary()).
-type finalizer() :: fun(() -> binary()).
-type streamfunc() :: fun((any()) -> ok | { error, atom() }).
-type ibrowse_options() :: [{atom(), any()}].
-type rewrite_rules() :: [{any(), string()}].

-record(callbacks, {
        processor :: processor(),
        finalizer :: finalizer(),
        stream_next :: streamfunc(),
        stream_close :: streamfunc()
    }).

-record(state, {
        this_node :: binary(),
        enable_gzip :: boolean(),
        rewrite_rules :: rewrite_rules(),
        ibrowse_options :: ibrowse_options(),
        callbacks :: #callbacks{}
    }).

-define(SECRET_PROXY_HEADER, <<"x-erlang-http-proxy">>).
-define(HACKER_REDIRECT_PAGE, <<"http://www.fbi.gov/">>).

% copy-pasted from /usr/lib/erlang/lib/erts-5.9.1/src/zlib.erl
-define(MAX_WBITS, 15).

init(_Transport, Req, []) ->
    lager:debug("~p Initializing...", [self()]),
    { ok, EnableGzip } = application:get_env(http_proxy, enable_gzip),
    {Headers, _} = cowboy_req:headers(Req),
    AcceptGzip =
        case proplists:lookup(<<"accept-encoding">>, Headers) of
            none -> false;
            {_Key, AcceptEncoding} ->
                lists:any(
                    fun(X) -> X == "gzip" end,
                    string:tokens(
                        lists:filter(
                            fun(X) -> X =/= 16#20 end,
                            binary_to_list(AcceptEncoding)
                        ), ","
                    )
                )
        end, 
    State = #state {
        enable_gzip = EnableGzip andalso AcceptGzip,
        this_node = this_node(),
        rewrite_rules = init_rewrite_rules(),
        ibrowse_options = init_ibrowse_options(),
        callbacks = init_default_callbacks()
    },
    {ok, Req, State}.

handle(
        Req,
        #state {
            ibrowse_options = IBrowseOptions,
            rewrite_rules = RewriteRules,
            this_node = ThisNode
        } = State) ->
    {Headers, _} = cowboy_req:headers(Req),
    {Method, _} = cowboy_req:method(Req),
    {ok, Body, _} = cowboy_req:body(Req),
    Url = case proplists:lookup(?SECRET_PROXY_HEADER, Headers) of
            {?SECRET_PROXY_HEADER, ThisNode} ->
                {Peer, _} =  cowboy_req:peer(Req),
                lager:warning("~p Recursive request from ~p!", [self(), Peer]),
                ?HACKER_REDIRECT_PAGE;
            none -> 
                { ReqUrl, _ } = cowboy_req:url(Req),
                RewriteResult = apply_rewrite_rules(ReqUrl, RewriteRules),
                case ReqUrl == RewriteResult of
                    true -> ok;
                    false ->
                        lager:debug("~p Request URL: ~p", [self(), ReqUrl])
                end,
                RewriteResult
        end,
    lager:debug("~p Fetching ~s", [self(), Url]),
    ModifiedHeaders = modify_req_headers(Headers, ThisNode),
    {ibrowse_req_id, _RequestId} = ibrowse:send_req(
        binary_to_list(Url),
        headers_cowboy_to_ibrowse(ModifiedHeaders),
        req_type_cowboy_to_ibrowse(Method),
        Body,
        IBrowseOptions,
        infinity
    ),

    FinalReq = receive_loop(State, Req),
    lager:debug("~p Done", [self()]),
    {ok, FinalReq, State}.

terminate(_Req, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec init_rewrite_rules() -> rewrite_rules().
init_rewrite_rules() ->
    { ok, RewriteRules } = application:get_env(http_proxy, rewrite_rules),
    lists:map(
        fun({ReString,ReplaceString}) ->
            {ok, CompiledRe} = re:compile(ReString),
            {CompiledRe, ReplaceString}
        end,
        RewriteRules
    ).

-spec init_ibrowse_options() -> ibrowse_options().
init_ibrowse_options() ->
    { ok, SyncStream } = application:get_env(http_proxy, sync_stream),
    OptionsTemplate = [{ response_format, binary }],
    case SyncStream of
        true ->
            [ {stream_to, {self(), once}} | OptionsTemplate ];
        false ->
            { ok, ChunkSize } = application:get_env(
                http_proxy, stream_chunk_size),
            OptionsTemplate ++ [
                { stream_to, self() },
                { stream_chunk_size, ChunkSize }
            ]
    end.

-spec init_default_callbacks() -> #callbacks{}.
init_default_callbacks() -> 
    { ok, SyncStream } = application:get_env(http_proxy, sync_stream),
    CallbacksTemplate = #callbacks {
            processor = fun(X) -> X end,
            finalizer = fun() -> <<>> end,
            stream_next = fun(_ReqId) -> ok end,
            stream_close = fun(_ReqId) -> ok end
        },
    case SyncStream of
        true ->
            CallbacksTemplate#callbacks {
                stream_next = fun(ReqId) -> ibrowse:stream_next(ReqId) end,
                stream_close = fun(ReqId) -> ibrowse:stream_close(ReqId) end
            };
        false ->
            CallbacksTemplate
    end.

receive_loop(
        #state { 
            enable_gzip = EnableGzip,
            callbacks = #callbacks {
                processor = Processor,
                finalizer = Finalizer,
                stream_next = StreamNext,
                stream_close = StreamClose
            } = Callbacks
        } = State,
        Req) ->
    receive
        { ibrowse_async_headers, RequestId, Code, IBrowseHeaders } ->
            ok = StreamNext(RequestId),
            Headers = headers_ibrowse_to_cowboy(IBrowseHeaders),
            ModifiedHeaders = modify_res_headers(Headers),

            { NewHeaders, NewCallbacks} = 
                case EnableGzip of
                    true ->
                        optional_add_gzip_compression(
                            ModifiedHeaders, Callbacks
                        );
                    false ->
                        { ModifiedHeaders, Callbacks }
                end,
            { ok, NewReq } = send_headers(Req, Code, NewHeaders),
            receive_loop(State#state { callbacks = NewCallbacks }, NewReq);
        { ibrowse_async_response, RequestId, Data } ->
            ok = StreamNext(RequestId),
            ok = send_chunk(Req, Processor(Data)),
            receive_loop(State, Req);

        { ibrowse_async_response_end, RequestId } ->
            ok = StreamClose(RequestId),
            ok = send_chunk(Req, Finalizer()),
            Req 
    end.

-spec send_chunk(request(), binary()) -> ok | {error, atom()}.
send_chunk(Req, Data) ->
    case Data of
        <<>> -> ok;
        _ ->
            cowboy_req:chunk(Data, Req)
    end.

-spec apply_rewrite_rules(binary(), rewrite_rules()) -> binary().
apply_rewrite_rules(Url, []) ->
    Url;
apply_rewrite_rules(Url, [{CompiledRe,ReplaceString}|OtherRules]) ->
    ApplyResult = re:replace(Url, CompiledRe, ReplaceString),
    case is_list(ApplyResult) of
        true -> iolist_to_binary(ApplyResult);
        false -> apply_rewrite_rules(Url, OtherRules)
    end.

-spec optional_add_gzip_compression(headers(), #callbacks{}) -> { headers(), #callbacks{} }.
optional_add_gzip_compression(Headers, Callbacks) ->
    case proplists:get_value(<<"content-encoding">>, Headers) of 
        undefined ->
            lager:debug("~p Using gzip compression", [self()]),
            ZlibStream = zlib:open(),
            ok = zlib:deflateInit(ZlibStream, default, deflated, 16+?MAX_WBITS, 8, default),
            {
                 [ {<<"content-encoding">>, <<"gzip">>} | Headers ],
                 Callbacks#callbacks {
                     processor =
                     fun(<<>>) -> <<>>;
                        (Data) ->
                         iolist_to_binary(
                             zlib:deflate(ZlibStream, Data, sync)
                         )
                     end,
                     finalizer = fun() ->
                         Data = iolist_to_binary(
                              zlib:deflate(ZlibStream, <<>>, finish)
                         ),
                         ok = zlib:deflateEnd(ZlibStream),
                         ok = zlib:close(ZlibStream),
                         Data
                     end
                }
            };
        _Other ->
            { Headers, Callbacks }
    end.

-spec send_headers(request(), string(), headers()) -> { ok, request() }.
send_headers(Req, Code, Headers) ->
    NewReq = lists:foldl(
        fun({HeaderName, HeaderValue}, Acc) ->
            cowboy_req:set_resp_header(
                HeaderName,
                HeaderValue,
                Acc)
        end,
        Req,
        Headers
    ),
    cowboy_req:chunked_reply(list_to_integer(Code), NewReq).

-spec modify_req_headers(headers(), binary()) -> headers().
modify_req_headers(Headers, ThisNode) ->
    FilteredHeaders = lists:filter(
        fun({<<"proxy-connection">>, _}) -> false;
           ({?SECRET_PROXY_HEADER, _}) -> false;
           ({<<"host">>, _}) -> false;
           ({_, _}) -> true
        end,
        Headers
    ),
    [ {?SECRET_PROXY_HEADER, ThisNode}
        | FilteredHeaders].

-spec modify_res_headers(headers()) -> headers().
modify_res_headers(Headers) ->
    lists:filter(
        fun({<<"date">>, _}) -> false;
           ({<<"transfer-encoding">>, _}) -> false;
           ({<<"connection">>, _}) -> false;
           ({<<"server">>, _}) -> false;
           ({<<"content-length">>, _}) -> false;
           ({_, _}) -> true
        end,
        Headers
    ).

-spec req_type_cowboy_to_ibrowse(binary()) -> get | head | post.
req_type_cowboy_to_ibrowse(RequestBinary) ->
    case string:to_lower(binary_to_list(RequestBinary)) of
        "post" -> post;
        "head" -> head;
         _Other -> get
    end.

-spec headers_ibrowse_to_cowboy([{string(),string()}]) -> headers().
headers_ibrowse_to_cowboy(Headers) ->
    lists:map(
            fun({K,V}) -> 
                { list_to_binary(string:to_lower(K)), list_to_binary(V) }
            end,
            Headers
    ). 

-spec headers_cowboy_to_ibrowse(headers()) -> [{string(),string()}].
headers_cowboy_to_ibrowse(Headers) ->
    lists:map(
            fun({K,V}) -> 
                { binary_to_list(K), binary_to_list(V) }
            end,
            Headers
    ).

-spec this_node() -> binary().
this_node() ->
    [Node, Host] = string:tokens(atom_to_list(node()), "@"),
    iolist_to_binary([Node, "/", Host]).
