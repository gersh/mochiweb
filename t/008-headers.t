#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(11),
    H = mochiweb_headers:make([{hdr, foo}, {"Hdr", "bar"}, {'Hdr', 2}]),
    etap:is([{hdr, "foo, bar, 2"}], mochiweb_headers:to_list(H), "to_list/1"),

    H1 = mochiweb_headers:insert(taco, grande, H),
    etap:is([{hdr, "foo, bar, 2"}, {taco, "grande"}], mochiweb_headers:to_list(H1), "to_list/1"),

    H2 = mochiweb_headers:make([{"Set-Cookie", "foo"}]),
    etap:is([{"Set-Cookie", "foo"}], mochiweb_headers:to_list(H2), "to_list/1"),

    H3 = mochiweb_headers:insert("Set-Cookie", "bar", H2),
    etap:is([{"Set-Cookie", "foo"}, {"Set-Cookie", "bar"}], mochiweb_headers:to_list(H3), "to_list/1"),
    etap:is("foo, bar", mochiweb_headers:get_value("set-cookie", H3), "to_list/1"),
    etap:is({value, {"Set-Cookie", "foo, bar"}}, mochiweb_headers:lookup("set-cookie", H3), "lookup/2"),
    etap:is(undefined, mochiweb_headers:get_value("fuzzy", H3), "get_value/2"),
    etap:is(none, mochiweb_headers:lookup("shibby", H3), "lookup/2"),
    
    H4 = mochiweb_headers:insert("content-type", "application/x-www-form-urlencoded; charset=utf8", H3),
    etap:is("application/x-www-form-urlencoded", mochiweb_headers:get_primary_value("content-type", H4), "get_primary_value/2"),
    etap:is(H4, mochiweb_headers:delete_any("nonexistent-header", H4), "delete_any/2"),
    etap:is(H3, mochiweb_headers:delete_any("content-type", H4), "delete_any/2"),

    etap:end_tests().
