-record(hello_msg, {
    handler :: pid(),
    peer    :: term(),
    message :: binary()
}).

-record(hello_closed, {
    handler  :: pid(),
    peer     :: term()
}).
