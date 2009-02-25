#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl

main(_) ->
    etap:plan(10),
    
    %% destack/1 isn't exported: to test or to not test, that is the question.
    % etap:is({<<"a">>, [], []}, mochiweb_html:destack([{<<"a">>, [], []}]), "destack/1"),
    % etap:is({<<"a">>, [], [{<<"b">>, [], []}]}, mochiweb_html:destack([{<<"b">>, [], []}, {<<"a">>, [], []}]), "destack/1"),
    % etap:is({<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]}, mochiweb_html:destack([{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}]), "destack/1"),
    % etap:is([{<<"a">>, [], [{<<"b">>, [], [{<<"c">>, [], []}]}]}], mochiweb_html:destack(<<"b">>, [{<<"c">>, [], []}, {<<"b">>, [], []}, {<<"a">>, [], []}]), "destack/1"),
    % etap:is([{<<"b">>, [], [{<<"c">>, [], []}]}, {<<"a">>, [], []}], mochiweb_html:destack(<<"c">>, [{<<"c">>, [], []}, {<<"b">>, [], []},{<<"a">>, [], []}]), "destack/1"),
    
    etap:is(
        <<"<html><head><title>hey!</title></head><body><p class=\"foo\">what's up<br /></p><div>sucka</div><!-- comment! --></body></html>">>,
        iolist_to_binary(
                   mochiweb_html:to_html({html, [],
                            [{<<"head">>, [],
                              [{title, <<"hey!">>}]},
                             {body, [],
                              [{p, [{class, foo}], [<<"what's">>, <<" up">>, {br}]},
                               {'div', <<"sucka">>},
                               {comment, <<" comment! ">>}]}]})),
        "to_html/1"
    ),
    etap:is(
        <<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">">>,
        iolist_to_binary(
                mochiweb_html:to_html({doctype,
                         [<<"html">>, <<"PUBLIC">>,
                          <<"-//W3C//DTD XHTML 1.0 Transitional//EN">>,
                          <<"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">>]})),
        "to_html/1"
    ),
    
    etap:is(<<"&amp;quot;\"word &lt;&lt;up!&amp;quot;">>, mochiweb_html:escape(<<"&quot;\"word <<up!&quot;">>), "escape/1"),
    etap:is(<<"&amp;quot;&quot;word &lt;&lt;up!&amp;quot;">>, mochiweb_html:escape_attr(<<"&quot;\"word <<up!&quot;">>), "escape_attr/1"),    
    
    etap:is(
        [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>}, {<<"wibble">>, <<"wibble">>}, {<<"alice">>, <<"bob">>}], true}],
        mochiweb_html:tokens(<<"<foo bar=baz wibble='wibble' alice=\"bob\"/>">>),
        "tokens/1"
    ),
    etap:is(
        [{start_tag, <<"foo">>, [{<<"bar">>, <<"baz">>}, {<<"wibble">>, <<"wibble">>}, {<<"alice">>, <<"bob">>}], true}],
        mochiweb_html:tokens(<<"<foo bar=baz wibble='wibble' alice=bob/>">>),
        "tokens/1"
    ),
    etap:is(
        [{comment, <<"[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]">>}],
        mochiweb_html:tokens(<<"<!--[if lt IE 7]>\n<style type=\"text/css\">\n.no_ie { display: none; }\n</style>\n<![endif]-->">>),
        "tokens/1"
    ),
    etap:is(
        [{start_tag, <<"script">>, [{<<"type">>, <<"text/javascript">>}], false},{data, <<" A= B <= C ">>, false}, {end_tag, <<"script">>}],
        mochiweb_html:tokens(<<"<script type=\"text/javascript\"> A= B <= C </script>">>),
        "tokens/1"
    ),
    etap:is(
        [{start_tag, <<"textarea">>, [], false}, {data, <<"<html></body>">>, false}, {end_tag, <<"textarea">>}],
        mochiweb_html:tokens(<<"<textarea><html></body></textarea>">>),
        "tokens/1"
    ),
    
    D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org</link><description>Bob's Rants</description></channel>">>,
    Expect = [
        {start_tag,<<"channel">>,[],false},
        {start_tag,<<"title">>,[],false},
        {data,<<"from __future__ import *">>,false},
        {end_tag,<<"title">>},
        {start_tag,<<"link">>,[],true},
        {data,<<"http://bob.pythonmac.org">>,false},
        {end_tag,<<"link">>},
        {start_tag,<<"description">>,[],false},
        {data,<<"Bob's Rants">>,false},
        {end_tag,<<"description">>},
        {end_tag,<<"channel">>}
    ],
    etap:is(Expect, mochiweb_html:tokens(D0), "tokens/1"),
    
    etap:end_tests().

% test_parse() ->
%     D0 = <<"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
% <html>
%  <head>
%    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">
%    <title>Foo</title>
%    <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/rel/dojo/resources/dojo.css\" media=\"screen\">
%    <link rel=\"stylesheet\" type=\"text/css\" href=\"/static/foo.css\" media=\"screen\">
%    <!--[if lt IE 7]>
%    <style type=\"text/css\">
%      .no_ie { display: none; }
%    </style>
%    <![endif]-->
%    <link rel=\"icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
%    <link rel=\"shortcut icon\" href=\"/static/images/favicon.ico\" type=\"image/x-icon\">
%  </head>
%  <body id=\"home\" class=\"tundra\"><![CDATA[&lt;<this<!-- is -->CDATA>&gt;]]></body>
% </html>">>,
%     Expect = {<<"html">>, [],
%               [{<<"head">>, [],
%                 [{<<"meta">>,
%                   [{<<"http-equiv">>,<<"Content-Type">>},
%                    {<<"content">>,<<"text/html; charset=UTF-8">>}],
%                   []},
%                  {<<"title">>,[],[<<"Foo">>]},
%                  {<<"link">>,
%                   [{<<"rel">>,<<"stylesheet">>},
%                    {<<"type">>,<<"text/css">>},
%                    {<<"href">>,<<"/static/rel/dojo/resources/dojo.css">>},
%                    {<<"media">>,<<"screen">>}],
%                   []},
%                  {<<"link">>,
%                   [{<<"rel">>,<<"stylesheet">>},
%                    {<<"type">>,<<"text/css">>},
%                    {<<"href">>,<<"/static/foo.css">>},
%                    {<<"media">>,<<"screen">>}],
%                   []},
%                  {comment,<<"[if lt IE 7]>\n   <style type=\"text/css\">\n     .no_ie { display: none; }\n   </style>\n   <![endif]">>},
%                  {<<"link">>,
%                   [{<<"rel">>,<<"icon">>},
%                    {<<"href">>,<<"/static/images/favicon.ico">>},
%                    {<<"type">>,<<"image/x-icon">>}],
%                   []},
%                  {<<"link">>,
%                   [{<<"rel">>,<<"shortcut icon">>},
%                    {<<"href">>,<<"/static/images/favicon.ico">>},
%                    {<<"type">>,<<"image/x-icon">>}],
%                   []}]},
%                {<<"body">>,
%                 [{<<"id">>,<<"home">>},
%                  {<<"class">>,<<"tundra">>}],
%                 [<<"&lt;<this<!-- is -->CDATA>&gt;">>]}]},
%     Expect = parse(D0),
%     ok.

% 
% test_parse2() ->
%     D0 = <<"<channel><title>from __future__ import *</title><link>http://bob.pythonmac.org<br>foo</link><description>Bob's Rants</description></channel>">>,
%     Expect = {<<"channel">>,[],
%               [{<<"title">>,[],[<<"from __future__ import *">>]},
%                {<<"link">>,[],[
%                                <<"http://bob.pythonmac.org">>,
%                                {<<"br">>,[],[]},
%                                <<"foo">>]},
%                {<<"description">>,[],[<<"Bob's Rants">>]}]},
%     Expect = parse(D0),
%     ok.
% 
% test_parse_tokens() ->
%     D0 = [{doctype,[<<"HTML">>,<<"PUBLIC">>,<<"-//W3C//DTD HTML 4.01 Transitional//EN">>]},
%           {data,<<"\n">>,true},
%           {start_tag,<<"html">>,[],false}],
%     {<<"html">>, [], []} = parse_tokens(D0),
%     D1 = D0 ++ [{end_tag, <<"html">>}],
%     {<<"html">>, [], []} = parse_tokens(D1),
%     D2 = D0 ++ [{start_tag, <<"body">>, [], false}],
%     {<<"html">>, [], [{<<"body">>, [], []}]} = parse_tokens(D2),
%     D3 = D0 ++ [{start_tag, <<"head">>, [], false},
%                 {end_tag, <<"head">>},
%                 {start_tag, <<"body">>, [], false}],
%     {<<"html">>, [], [{<<"head">>, [], []}, {<<"body">>, [], []}]} = parse_tokens(D3),
%     D4 = D3 ++ [{data,<<"\n">>,true},
%                 {start_tag,<<"div">>,[{<<"class">>,<<"a">>}],false},
%                 {start_tag,<<"a">>,[{<<"name">>,<<"#anchor">>}],false},
%                 {end_tag,<<"a">>},
%                 {end_tag,<<"div">>},
%                 {start_tag,<<"div">>,[{<<"class">>,<<"b">>}],false},
%                 {start_tag,<<"div">>,[{<<"class">>,<<"c">>}],false},
%                 {end_tag,<<"div">>},
%                 {end_tag,<<"div">>}],
%     {<<"html">>, [],
%      [{<<"head">>, [], []},
%       {<<"body">>, [],
%        [{<<"div">>, [{<<"class">>, <<"a">>}], [{<<"a">>, [{<<"name">>, <<"#anchor">>}], []}]},
%         {<<"div">>, [{<<"class">>, <<"b">>}], [{<<"div">>, [{<<"class">>, <<"c">>}], []}]}
%        ]}]} = parse_tokens(D4),
%     D5 = [{start_tag,<<"html">>,[],false},
%           {data,<<"\n">>,true},
%           {data,<<"boo">>,false},
%           {data,<<"hoo">>,false},
%           {data,<<"\n">>,true},
%           {end_tag,<<"html">>}],
%     {<<"html">>, [], [<<"\nboohoo\n">>]} = parse_tokens(D5),
%     D6 = [{start_tag,<<"html">>,[],false},
%           {data,<<"\n">>,true},
%           {data,<<"\n">>,true},
%           {end_tag,<<"html">>}],
%     {<<"html">>, [], []} = parse_tokens(D6),
%     D7 = [{start_tag,<<"html">>,[],false},
%           {start_tag,<<"ul">>,[],false},
%           {start_tag,<<"li">>,[],false},
%           {data,<<"word">>,false},
%           {start_tag,<<"li">>,[],false},
%           {data,<<"up">>,false},
%           {end_tag,<<"li">>},
%           {start_tag,<<"li">>,[],false},
%           {data,<<"fdsa">>,false},
%           {start_tag,<<"br">>,[],true},
%           {data,<<"asdf">>,false},
%           {end_tag,<<"ul">>},
%           {end_tag,<<"html">>}],
%     {<<"html">>, [],
%      [{<<"ul">>, [],
%        [{<<"li">>, [], [<<"word">>]},
%         {<<"li">>, [], [<<"up">>]},
%         {<<"li">>, [], [<<"fdsa">>,{<<"br">>, [], []}, <<"asdf">>]}]}]} = parse_tokens(D7),
%     ok.
