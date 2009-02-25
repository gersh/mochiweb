#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin errlog_type error -boot start_sasl

main(_) ->
    etap:plan(14),

        %% RFC example
    C1a = "$Version=\"1\"; Customer=\"WILE_E_COYOTE\"; $Path=\"/acme\"; Part_Number=\"Rocket_Launcher_0001\"; $Path=\"/acme\"; Shipping=\"FedEx\"; $Path=\"/acme\"",
    etap:is(
        [{"Customer","WILE_E_COYOTE"}, {"Part_Number","Rocket_Launcher_0001"}, {"Shipping","FedEx"}],
        mochiweb_cookies:parse_cookie(C1a),
        "parse_cookies/1"
    ),
    etap:is([{"foo", "x"}], mochiweb_cookies:parse_cookie("foo=\"\\x\""), "parse_cookies/1"),
    etap:is([], mochiweb_cookies:parse_cookie("="), "parse_cookies/1"),
    etap:is([{"foo", ""}, {"bar", ""}], mochiweb_cookies:parse_cookie("  foo ; bar  "), "parse_cookies/1"),
    etap:is([{"foo", ""}, {"bar", ""}], mochiweb_cookies:parse_cookie("foo=;bar="), "parse_cookies/1"),
    etap:is([{"foo", "\";"}, {"bar", ""}], mochiweb_cookies:parse_cookie("foo = \"\\\";\";bar "), "parse_cookies/1"),
    etap:is([{"foo", "\";bar"}], mochiweb_cookies:parse_cookie("foo=\"\\\";bar"), "parse_cookies/1"),

    C1 = {"Set-Cookie", "Customer=WILE_E_COYOTE; Version=1; Path=/acme"},
    etap:is(C1, mochiweb_cookies:cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}]), "cookie/3"),
    etap:is(C1, mochiweb_cookies:cookie("Customer", "WILE_E_COYOTE", [{path, "/acme"}, {badoption, "negatory"}]), "cookie/3"),
    etap:is(C1, mochiweb_cookies:cookie('Customer', 'WILE_E_COYOTE', [{path, '/acme'}]), "cookie/3"),
    etap:is(C1, mochiweb_cookies:cookie(<<"Customer">>, <<"WILE_E_COYOTE">>, [{path, <<"/acme">>}]), "cookie/3"),

    etap:is({"Set-Cookie","=NoKey; Version=1"}, mochiweb_cookies:cookie("", "NoKey", []), "cookie/3"),

    LocalTime = calendar:universal_time_to_local_time({{2007, 5, 15}, {13, 45, 33}}), 
    C2 = {"Set-Cookie", "Customer=WILE_E_COYOTE; Version=1; Expires=Tue, 15 May 2007 13:45:33 GMT; Max-Age=0"},
    etap:is(C2, mochiweb_cookies:cookie("Customer", "WILE_E_COYOTE", [{max_age, -111}, {local_time, LocalTime}]), "cookies/3"),
    C3 = {"Set-Cookie", "Customer=WILE_E_COYOTE; Version=1; Expires=Wed, 16 May 2007 13:45:50 GMT; Max-Age=86417"},
    etap:is(C3, mochiweb_cookies:cookie("Customer", "WILE_E_COYOTE", [{max_age, 86417}, {local_time, LocalTime}]), "cookies/3"),
    
    etap:end_tests().
