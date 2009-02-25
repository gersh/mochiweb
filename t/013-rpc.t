#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(7),
    
    R1 = mochiweb_http:new_request({foo, {'POST', {abs_path, "/"}, {1,0}}, [{'Content-Type', "application/json"}]}),
    etap:is(ok, mochiweb_rpc:is_acceptable_request(R1), "acceptable_request/1"),

    R2 = mochiweb_http:new_request({foo, {'POST', {abs_path, "/"}, {1,1}}, [{'Content-Type', "application/json"}]}),
    etap:is(ok, mochiweb_rpc:is_acceptable_request(R2), "acceptable_request/1"),
    
    R3 = mochiweb_http:new_request({foo, {'POST', {abs_path, "/"}, {5,0}}, [{'Content-Type', "application/json"}]}),
    etap:is({status, 505, "HTTP Version {5,0} is not supported."}, mochiweb_rpc:is_acceptable_request(R3), "acceptable_request/1"),

    R4 = mochiweb_http:new_request({foo, {'PUT', {abs_path, "/"}, {1, 1}}, [{'Content-Type', "application/json"}]}),
    etap:is({status, 501, "The 'PUT' method has not been implemented."}, mochiweb_rpc:is_acceptable_request(R4), "acceptable_request/1"),

    R5 = mochiweb_http:new_request({foo, {'PUT', {abs_path, "/"}, {5,0}}, [{'Content-Type', "application/json"}]}),
    etap:is({status, 400, "Bad Request."}, mochiweb_rpc:is_acceptable_request(R5), "acceptable_request/1"),
    
    J1 = "{\"id\":\"foobarbaz\",\"method\":\"foo\",\"params\":{\"bar\":{\"baz\":[1,2,3]}}}",
    etap:is({ok, {call, foo, {struct,[{<<"bar">>,{struct,[{<<"baz">>,[1,2,3]}]}}]}, <<"foobarbaz">>}}, mochiweb_rpc:decode_request_body(J1), "decode_request_body/1"),

    J2 = "{\"id\":\"foobarbaz\"params\":{\"bar\":\"baz\"}}",
    etap:is({error, "Error decoding request."}, mochiweb_rpc:decode_request_body(J2), "decode_request_body/1"),
    
    etap:end_tests().
