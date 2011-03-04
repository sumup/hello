%    __                        __      _
%   / /__________ __   _____  / /___  (_)___  ____ _
%  / __/ ___/ __ `/ | / / _ \/ / __ \/ / __ \/ __ `/
% / /_/ /  / /_/ /| |/ /  __/ / /_/ / / / / / /_/ /
% \__/_/   \__,_/ |___/\___/_/ .___/_/_/ /_/\__, /
%                           /_/            /____/
%
% Copyright (c) Travelping GmbH <info@travelping.com>


-module(tp_json_rpc).

-export([start/0, handle_request/2, call/3, notification/3, call_np/3]).

-include("jrpc_internal.hrl").

-define(TIME_OUT, {timeout, 15000}).

start() ->
    application:start(inets),
    application:start(tp_json_rpc).

handle_request(Service, JSON) when is_list(JSON) ->
    Resp = case tpjrpc_proto:request_json(JSON) of
               {ok, Request}  ->
                   tp_json_rpc_service:handle_request(Service, Request);
               {batch, Valid, Invalid} ->
                   Resps = tp_json_rpc_service:handle_request(Service, Valid),
                   Invalid ++ Resps;
               {error, Error} ->
                   Error
           end,
    ResponseJSON = tpjrpc_proto:response_json(Resp),
    tpjrpc_logger:log(JSON,ResponseJSON),
    ResponseJSON.

rpc_request(HostURL, #request{id = Id, method = Method, params = ArgList}) ->
    Methodto    = into_bin(Method),
    IDField     = case Id of
                      undefined -> [];
                      _         -> [{"id", Id}]
                  end,
    RequestJSON = tpjrpc_json:encode({obj, IDField ++ [{"jsonrpc", <<"2.0">>}, {"version", <<"1.1">>}, {"method", Methodto}, {"params", ArgList}]}),
    {ok, Hostname, _Path} = split_url(HostURL),
    Headers     = [{"Host", Hostname}, {"Connection", "keep-alive"},
                   {"Content-Length", integer_to_list(iolist_size(RequestJSON))},
                   {"Content-Type", "application/json"},
                   {"Accept", "application/json"},
                   {"User-Agent", "tp_json_rpc"}],
    HTTPRequest = {HostURL, Headers, "application/json", RequestJSON},
    case httpc:request(post, HTTPRequest, [?TIME_OUT], [{headers_as_is, true}, {full_result, false}]) of
       {ok, {_Code, Body}} -> {ok, Body};
       {error, Reason}     -> {error, {http, Reason}}
    end.

call(Host, Method, ArgList) when is_list(ArgList) or is_tuple(ArgList) ->
    Request = #request{id = 1, method = Method, params = ArgList},
    case rpc_request(Host, Request) of
        {error, Error} -> {error, Error};
        {ok, Body} ->
            case tpjrpc_json:decode(Body) of
               {error, syntax_error} -> {error, syntax_error};
               {ok, {obj, Props}, _Rest} ->
                   case proplists:get_value("error", Props, null) of
                       null ->
                           Result = proplists:get_value("result", Props),
                           {ok, Result};
                       {obj, ErrorObject} ->
                           case proplists:get_value("code", ErrorObject) of
                               -32600 -> {error, invalid_request};
                               -32601 -> {error, method_not_found};
                               -32602 -> {error, invalid_params};
                               -32603 -> {error, internal_error};
                               Code when (Code >= -32099) and (Code =< -32000) -> {error, server_error};
                               Code -> {error, Code}
                           end
                   end
            end
    end.

notification(Host, Method, ArgList) ->
    case rpc_request(Host, #request{method=Method, params=ArgList}) of
        {error, Reason} -> {error, Reason};
        {ok, _Body}     -> ok
    end.

call_np(Host, Method, List) when is_list(List) ->
    ArgList={obj,List},
    call(Host, Method, ArgList).

into_bin(Bin) when is_list(Bin) -> list_to_binary(Bin);
into_bin(Bin)                   -> Bin.

split_url(URL) ->
    case re:run(URL, "^http://([^/]+)(.*)", [{capture, all_but_first, list}]) of
        {match, [Host, Path]} -> {ok, Host, Path};
        nomatch               -> {error, not_url}
    end.