#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(27),
    
    etap:is({exact, 0}, mochiweb_multipart:find_in_binary(<<"foo">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is({exact, 1}, mochiweb_multipart:find_in_binary(<<"oo">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is({exact, 8}, mochiweb_multipart:find_in_binary(<<"z">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is(not_found, mochiweb_multipart:find_in_binary(<<"q">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is({partial, 7, 2}, mochiweb_multipart:find_in_binary(<<"azul">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is({exact, 0}, mochiweb_multipart:find_in_binary(<<"foobarbaz">>, <<"foobarbaz">>), "find_in_binary/2"),
    etap:is({partial, 0, 3}, mochiweb_multipart:find_in_binary(<<"foobar">>, <<"foo">>), "find_in_binary/2"),
    etap:is({partial, 1, 3}, mochiweb_multipart:find_in_binary(<<"foobar">>, <<"afoo">>), "find_in_binary/2"),
    
	etap_exception:lives_ok(fun() ->
        B = <<"\r\n--X">>,
        etap:is({next_boundary, 0, 7}, mochiweb_multipart:find_boundary(B, <<"\r\n--X\r\nRest">>), "find_boundary/2"),
        etap:is({next_boundary, 1, 7}, mochiweb_multipart:find_boundary(B, <<"!\r\n--X\r\nRest">>), "find_boundary/2"),
        etap:is({end_boundary, 0, 9}, mochiweb_multipart:find_boundary(B, <<"\r\n--X--\r\nRest">>), "find_boundary/2"),
        etap:is({end_boundary, 1, 9}, mochiweb_multipart:find_boundary(B, <<"!\r\n--X--\r\nRest">>), "find_boundary/2"),
        etap:is(not_found, mochiweb_multipart:find_boundary(B, <<"--X\r\nRest">>), "find_boundary/2"),
        etap:is({maybe, 0}, mochiweb_multipart:find_boundary(B, <<"\r\n--X\r">>), "find_boundary/2"),
        etap:is({maybe, 1}, mochiweb_multipart:find_boundary(B, <<"!\r\n--X\r">>), "find_boundary/2"),
        P = <<"\r\n-----------------------------16037454351082272548568224146">>,
        B0 = <<55,212,131,77,206,23,216,198,35,87,252,118,252,8,25,211,132,229,
              182,42,29,188,62,175,247,243,4,4,0,59, 13,10,45,45,45,45,45,45,45,
              45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,
              49,54,48,51,55,52,53,52,51,53,49>>,
        etap:is({maybe, 30}, mochiweb_multipart:find_boundary(P, B0), "find_boundary/2"),
        ok
    end, "find_boundry ok"),
    
	etap_exception:lives_ok(fun() ->
        ContentType = "multipart/form-data; boundary=AaB03x",
        etap:is("AaB03x", mochiweb_multipart:get_boundary(ContentType), "get_boundry/1"),
        Content = mochiweb_util:join([
            "--AaB03x",
            "Content-Disposition: form-data; name=\"submit-name\"",
            "",
            "Larry",
            "--AaB03x",
            "Content-Disposition: form-data; name=\"files\";"
            ++ "filename=\"file1.txt\"",
            "Content-Type: text/plain",
            "",
            "... contents of file1.txt ...",
            "--AaB03x--",
            ""
        ], "\r\n"),
        BinContent = iolist_to_binary(Content),
        Expect = [
            {headers, [{"content-disposition", {"form-data", [{"name", "submit-name"}]}}]},
            {body, <<"Larry">>},
            body_end,
            {headers, [{"content-disposition", {"form-data", [{"name", "files"}, {"filename", "file1.txt"}]}}, {"content-type", {"text/plain", []}}]},
            {body, <<"... contents of file1.txt ...">>},
            body_end,
            eof
        ],
        TestCallback = fun (Next) -> test_callback(Next, Expect) end,
        ServerFun = fun (Socket) ->
            case gen_tcp:send(Socket, BinContent) of
                ok -> exit(normal)
            end
        end,
        ClientFun = fun (Socket) ->
            Req = fake_request(Socket, ContentType, size(BinContent)),
            etap:is({0, <<>>, ok}, mochiweb_multipart:parse_multipart_request(Req, TestCallback), "parse_multipart_request/2"),
            ok
        end,
        etap:is(ok, with_socket_server(ServerFun, ClientFun), "with_socket_server/2"),
        ok
    end, "basic parse ok"),
    
	etap_exception:lives_ok(fun() ->
        ContentType = "multipart/form-data; boundary=---------------------------6072231407570234361599764024",
        BinContent = <<"-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------6072231407570234361599764024\r\nContent-Disposition: form-data; name=\"file\"; filename=\"\"\r\nContent-Type: application/octet-stream\r\n\r\n\r\n-----------------------------6072231407570234361599764024--\r\n">>,
        Expect = [
            {headers, [{"content-disposition", {"form-data", [{"name", "hidden"}]}}]},
            {body, <<"multipart message">>},
            body_end,
            {headers, [{"content-disposition", {"form-data", [{"name", "file"}, {"filename", ""}]}}, {"content-type", {"application/octet-stream", []}}]},
            {body, <<>>},
            body_end,
            eof
        ],
        TestCallback = fun (Next) -> test_callback(Next, Expect) end,
        ServerFun = fun (Socket) ->
            case gen_tcp:send(Socket, BinContent) of
                ok -> exit(normal)
            end
        end,
        ClientFun = fun (Socket) ->
            Req = fake_request(Socket, ContentType, size(BinContent)),
            etap:is({0, <<>>, ok}, mochiweb_multipart:parse_multipart_request(Req, TestCallback), "parse_multipart_request/2"),
            ok
        end,
        etap:is(ok, with_socket_server(ServerFun, ClientFun), "with_socket_server/2"),
        ok
    end, "parse 2 ok"),
    
	etap_exception:lives_ok(fun() ->
        ContentType = "multipart/form-data; boundary=---------------------------7386909285754635891697677882",
        BinContent = <<"-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"hidden\"\r\n\r\nmultipart message\r\n-----------------------------7386909285754635891697677882\r\nContent-Disposition: form-data; name=\"file\"; filename=\"test_file.txt\"\r\nContent-Type: text/plain\r\n\r\nWoo multiline text file\n\nLa la la\r\n-----------------------------7386909285754635891697677882--\r\n">>,
        Expect = [
            {headers, [{"content-disposition", {"form-data", [{"name", "hidden"}]}}]},
            {body, <<"multipart message">>},
            body_end,
            {headers, [{"content-disposition", {"form-data", [{"name", "file"}, {"filename", "test_file.txt"}]}}, {"content-type", {"text/plain", []}}]},
            {body, <<"Woo multiline text file\n\nLa la la">>},
            body_end,
            eof
        ],
        TestCallback = fun (Next) -> test_callback(Next, Expect) end,
        ServerFun = fun (Socket) ->
            case gen_tcp:send(Socket, BinContent) of
                ok -> exit(normal)
            end
        end,
        ClientFun = fun (Socket) ->
            Req = fake_request(Socket, ContentType, size(BinContent)),
            etap:is({0, <<>>, ok}, mochiweb_multipart:parse_multipart_request(Req, TestCallback), "parse_multipart_request/2"),
            ok
        end,
        etap:is(ok, with_socket_server(ServerFun, ClientFun), "with_socket_server/2"),
        ok
    end, "parse 3 ok"),


    etap:end_tests().

fake_request(Socket, ContentType, Length) ->
    mochiweb_request:new(Socket, 'POST', "/multipart", {1,1},
        mochiweb_headers:make([{"content-type", ContentType}, {"content-length", Length}])).

test_callback(Expect, [Expect | Rest]) ->
    case Rest of
        [] -> ok;
        _ -> fun (Next) -> test_callback(Next, Rest) end
    end.

with_socket_server(ServerFun, ClientFun) ->
    {ok, Server} = mochiweb:start([{ip, "127.0.0.1"}, {port, 0}, {loop, ServerFun}]),
    Port = mochiweb:get(Server, port),
    {ok, Client} = gen_tcp:connect("127.0.0.1", Port, [binary, {active, false}]),
    Res = (catch ClientFun(Client)),
    mochiweb:stop(Server),
    Res.
