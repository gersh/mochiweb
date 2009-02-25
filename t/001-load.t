#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(20),
    etap_can:loaded_ok(mochifmt, "Module 'mochifmt' loaded"),
    etap_can:loaded_ok(mochifmt_records, "Module 'mochifmt_records' loaded"),
    etap_can:loaded_ok(mochifmt_std, "Module 'mochifmt_std' loaded"),
    etap_can:loaded_ok(mochihex, "Module 'mochihex' loaded"),
    etap_can:loaded_ok(mochijson, "Module 'mochijson' loaded"),
    etap_can:loaded_ok(mochijson2, "Module 'mochijson2' loaded"),
    etap_can:loaded_ok(mochinum, "Module 'mochinum' loaded"),
    etap_can:loaded_ok(mochiweb, "Module 'mochiweb' loaded"),
    etap_can:loaded_ok(mochiweb_charref, "Module 'mochiweb_charref' loaded"),
    etap_can:loaded_ok(mochiweb_cookies, "Module 'mochiweb_cookies' loaded"),
    etap_can:loaded_ok(mochiweb_echo, "Module 'mochiweb_echo' loaded"),
    etap_can:loaded_ok(mochiweb_headers, "Module 'mochiweb_headers' loaded"),
    etap_can:loaded_ok(mochiweb_html, "Module 'mochiweb_html' loaded"),
    etap_can:loaded_ok(mochiweb_http, "Module 'mochiweb_http' loaded"),
    etap_can:loaded_ok(mochiweb_multipart, "Module 'mochiweb_multipart' loaded"),
    etap_can:loaded_ok(mochiweb_request, "Module 'mochiweb_request' loaded"),
    etap_can:loaded_ok(mochiweb_response, "Module 'mochiweb_response' loaded"),
    %% etap_can:loaded_ok(mochiweb_rpc, "Module 'mochiweb_rpc' loaded"),
    etap_can:loaded_ok(mochiweb_skel, "Module 'mochiweb_skel' loaded"),
    etap_can:loaded_ok(mochiweb_util, "Module 'mochiweb_util' loaded"),
    etap_can:loaded_ok(reloader, "Module 'reloader' loaded"),
    etap:end_tests().
