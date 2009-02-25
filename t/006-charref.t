#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(5),
    etap:is(1234, mochiweb_charref:charref("#1234"), "charref/1"),
    etap:is(255, mochiweb_charref:charref("#xfF"), "charref/1"),
    etap:is(255, mochiweb_charref:charref("#XFf"), "charref/1"),
    etap:is(38, mochiweb_charref:charref("amp"), "charref/1"),
    etap:is(undefined, mochiweb_charref:charref("not_an_entity"), "charref/1"),
    etap:end_tests().
