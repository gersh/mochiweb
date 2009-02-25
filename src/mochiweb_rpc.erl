%% @author Abhay Kumar <abhay@opensynapse.net>
%% @copyright 2008 Abhay Kumar

%% @doc Generic RPC Implementation. Only supports JSON-RPC for now and does not support sessions.

-module(mochiweb_rpc).
-author("Abhay Kumar <abhay@opensynapse.net>").

-export([handler/2]).
-export([is_acceptable_request/1, decode_request_body/1]).

%% @spec handler(Request, {Module, Function}) -> MochiWebResponse
%% @doc Use this function in order to process RPC calls from the outside.
%% Write your own function to process the Erlang representation of the 
%% RPC objects and pass it into this function as a {Module, Function}
%% tuple.
handler(Req, ModFun) ->
  case is_acceptable_request(Req) of
    ok ->
      handle_request(Req, ModFun);
    {status, StatusCode, Reason} ->
      send(Req, StatusCode, encode_error(Reason, null))
    end.

is_acceptable_request(Req) ->
  case {Req:get(method), Req:get(version)} of
    {'POST', {1, 0}} -> ok;
    {'POST', {1, 1}} -> ok;
    {'POST', HTTPVersion} -> {status, 505, lists:flatten(io_lib:format("HTTP Version ~p is not supported.", [HTTPVersion]))};
    {Method, {1, 1}} -> {status, 501, lists:flatten(io_lib:format("The ~p method has not been implemented.", [Method]))};
    _ -> {status, 400, "Bad Request."}
  end.

handle_request(Req, {Mod, Fun}) ->
    Body = binary_to_list(Req:recv_body()),
    case decode_request_body(Body) of
      {ok, Decoded, ID} ->
        case Mod:Fun(Req, Decoded) of
          {'EXIT', Reason} ->
            send(Req, 500, encode_error(Reason, ID));
          {error, Reason} ->
            send(Req, 500, encode_error(Reason, ID));
          {result, Result} ->
            send(Req, 200, encode_result(Result, ID))
          end;
      {error, Reason} ->
        send(Req, 500, encode_error(Reason, null))
    end.

decode_request_body(Body) ->
  try
    JsonObj = mochijson2:decode(Body),
    {ok, {call, list_to_atom(binary_to_list(fetch(JsonObj, method))), fetch(JsonObj, params), fetch(JsonObj, id)}}
  catch
     _:_ -> {error, "Error decoding request."}
  end.

encode_error(Reason, ID) -> lists:flatten(mochijson2:encode({struct, [{id, ID}, {error, Reason}, {result, null}]})).

encode_result(Result, ID) ->
  try
    lists:flatten(mochijson2:encode({struct, [{id, ID}, {error, null}, {result, Result}]}))
  catch
    _:_ -> encode_error("Error encoding response.", ID)
  end.

send(Req, StatusCode, JsonStr) ->
  Req:respond({StatusCode, [{'Content-Type', "application/json"}], JsonStr}).

fetch(JsonObj, Key) when is_atom(Key) ->
  fetch(JsonObj, list_to_binary(atom_to_list(Key)));
fetch({struct, List}, Key) when is_list(List) and is_binary(Key) ->
  case lists:keysearch(Key, 1, List) of
    {value, {Key, Value}} -> Value;
    _ -> []
  end.