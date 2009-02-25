#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin errlog_type error -boot start_sasl

main(_) ->
    etap:plan(4),
    etap:is(mochihex:to_hex([255, 0, 15, 241]), "ff000ff1", "to_hex/1"),
    etap:is(mochihex:to_bin("ff000ff1"), <<255, 0, 15, 241>>, "to_bin/1"),
    etap:is(mochihex:to_int("ff000ff1"), 16#ff000ff1, "to_int/1"),
    etap:is(mochihex:to_hex(16#ff000ff1), "ff000ff1", "to_hex/1"),
    etap:end_tests().
