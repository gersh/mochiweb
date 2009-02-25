#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl
main(_) ->
    etap:plan(skip),
    _ = fun() ->
        etap:is([{20, 30}], mochiweb_request:parse_range_request("bytes=20-30"), ""),
        etap:is([{20, none}], mochiweb_request:parse_range_request("bytes=20-"), ""),
        etap:is([{none, 20}], mochiweb_request:parse_range_request("bytes=-20"), ""),
        etap:is(fail, mochiweb_request:parse_range_request(""), ""),
        etap:is(fail, mochiweb_request:parse_range_request("garbage"), ""),
        etap:is(fail, mochiweb_request:parse_range_request("bytes=-20-30"), ""),
        etap:is([{20, 30}, {50, 100}, {110, 200}], mochiweb_request:parse_range_request("bytes=20-30,50-100,110-200"), ""),
        etap:is([{20, none}, {50, 100}, {none, 200}], mochiweb_request:parse_range_request("bytes=20-,50-100,-200"), ""),
        etap:is([], mochiweb_request:parse_range_request("bytes="), ""),
        Body = <<"012345678901234567890123456789012345678901234567890123456789">>,
        BodySize = size(Body), %% 60
        etap:is(BodySize, 60, ""),
        etap:is({1,9}, mochiweb_request:range_skip_length({1,9}, BodySize), ""),
        etap:is({10,10}, mochiweb_request:range_skip_length({10,19}, BodySize), ""),
        etap:is({40, 20}, mochiweb_request:range_skip_length({none, 20}, BodySize), ""),
        etap:is({30, 30}, mochiweb_request:range_skip_length({30, none}, BodySize), ""),
        etap:is({BodySize, 0}, mochiweb_request:range_skip_length({none, 0}, BodySize), ""),
        etap:is({0, BodySize}, mochiweb_request:range_skip_length({none, BodySize}, BodySize), ""),
        etap:is({0, BodySize}, mochiweb_request:range_skip_length({0, none}, BodySize), ""),
        BodySizeLess1 = BodySize - 1,
        etap:is({BodySizeLess1, 1}, mochiweb_request:range_skip_length({BodySize - 1, none}, BodySize), ""),
        etap:is({0, BodySize}, mochiweb_request:range_skip_length({none, BodySize + 1}, BodySize), ""),
        etap:is({0, BodySize}, mochiweb_request:range_skip_length({none, -1}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({-1, 30}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({0, BodySize + 1}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({-1, BodySize + 1}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({BodySize, 40}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({-1, none}, BodySize), ""),
        etap:is(invalid_range, mochiweb_request:range_skip_length({BodySize, none}, BodySize), "")
    end,
    etap:end_tests().
