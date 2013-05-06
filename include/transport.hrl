-record(hello_msg, {
    handler :: pid(),
    peer    :: term(),
    message :: binary(),
    closed = false :: boolean() %% if this is true, hello_closed will not be sent
}).

-record(hello_closed, {
    handler  :: pid(),
    peer     :: term()
}).
