-module(helloweb).
-export([start/0, stop/0]).

start() ->
    mochiweb_http:start([
        {name, ?MODULE},
        {ip, any},
        {port, 6500},
        {loop, fun(Req) ->
            error_logger:info_report([helloweb, {req, Req}]),
            Req:ok({"text/plain", "hello world"})
        end}
    ]).

stop() ->
    mochiweb:stop(?MODULE).
