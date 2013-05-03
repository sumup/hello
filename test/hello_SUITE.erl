-module(hello_SUITE).
-compile(export_all).

-include("ct.hrl").
-include("../include/hello.hrl").

% ---------------------------------------------------------------------
% -- test cases
bind_stateless_http_url_errors(_Config) ->
    URL = "http://127.0.0.1:5673/test",
    ok = hello:bind_stateless(URL, hello_stateless_handler_example),

    %% binding the same module returns already_started
    {error, already_started} = hello:bind_stateless(URL, hello_stateless_handler_example),

    %% binding a different one returns occupied
    {error, occupied} = hello:bind_stateless(URL, ?MODULE).

bind_stateless_zmq_url(_Config) ->
    ok = hello:bind_stateless("zmq-tcp://127.0.0.1:6001", hello_stateless_handler_example).

bind_stateless_zmq_url_errors(_Config) ->
    URL = "zmq-tcp://127.0.0.1:6002",
    ok = hello:bind_stateless(URL, hello_stateless_handler_example),

    %% binding the same module returns already_started
    {error, already_started} = hello:bind_stateless(URL, hello_stateless_handler_example),

    %% binding a different one returns occupied
    {error, occupied} = hello:bind_stateless(URL, ?MODULE).

bind_stateless_cross_protocol_checking(_Config) ->
    ok = hello:bind_stateless("http://localhost:6008", hello_stateless_handler_example),
    {error, occupied} = hello:bind_stateful("zmq-tcp://127.0.0.1:6008", hello_stateful_handler_example, []),

    ok = hello:bind_stateless("zmq-tcp://*:6009", hello_stateless_handler_example),
    {error, occupied} = hello:bind_stateful("http://127.0.0.1:6009", hello_stateful_handler_example, []).

bindings(_Config) ->
    OrigBindings = lists:sort(hello:bindings()),

    IPCPath = filename:absname("/tmp/bindings_test.ipc"),

    Bindings = [{"http://127.0.0.1:6003/test_1", hello_stateless_handler_example},
                {"http://127.0.0.1:6003/test_2", hello_stateless_handler_example},
                {"zmq-ipc://" ++ IPCPath,        hello_stateless_handler_example},
                {"zmq-tcp://127.0.0.1:6004",     hello_stateless_handler_example}],

    lists:foreach(fun ({URL, Module}) ->
                          ok = hello:bind_stateless(URL, Module)
                  end, Bindings),

    Bindings = lists:sort(hello:bindings() -- OrigBindings).

% ---------------------------------------------------------------------
% -- common_test callbacks
all() ->
    [bindings,
     bind_stateless_http_url_errors,
     bind_stateless_zmq_url, bind_stateless_zmq_url_errors,
     bind_stateless_cross_protocol_checking].

init_per_suite(Config) ->
    hello:start(),
    Config.

end_per_suite(_Config) ->
    application:stop(hello).
