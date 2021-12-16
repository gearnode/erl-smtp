%% Copyright (c) 2021 Exograd SAS.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(smtp_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_reply_test() ->
  Parser = smtp_parser:new(reply),

  %% One liner and complete reply
  ?assertEqual({ok,
                {220, [<<"mail.example.com ESMTP Postfix">>]},
                #{data => <<>>, state => initial, msg_type => reply}},
               smtp_parser:parse(Parser, <<"220 mail.example.com ESMTP Postfix\r\n">>)),

  %% Empty line
  ?assertEqual({more, Parser#{state => reply_line}},
               smtp_parser:parse(Parser, <<>>)),

  %% Not complete reply line
  {more, Parser2} = smtp_parser:parse(Parser, <<"220 mail">>),
  {ok, Reply, Parser3} =
    smtp_parser:parse(Parser2, <<".example.com ESMTP Postfix\r\n">>),

  ?assertEqual({220, [<<"mail.example.com ESMTP Postfix">>]},
               Reply),
  ?assertEqual(#{data => <<>>, state => initial, msg_type => reply},
               Parser3),

  %% Multiline
  MultiLineReply = <<"220-mail.example.com ESMTP Postfix\r\n220-\r\n220-System Info:  This is a Postfix mail server\r\n220-.             running a multiline SMTP greeting patch\r\n220-\r\n220-Further Info: See http://www.postfix.org\r\n220-\r\n220-Site Contact: John Smith, Postmaster\r\n220-Email:        <postmaster@example.com>\r\n220-Telephone:    +1-123-456-7890\r\n220-\r\n220 Please don't send me SPAM here - we don't like it\r\n">>,
  ?assertEqual({ok,
                {220,
                 [<<"mail.example.com ESMTP Postfix">>,
                  <<"">>,
                  <<"System Info:  This is a Postfix mail server">>,
                  <<".             running a multiline SMTP greeting patch">>,
                  <<"">>,
                  <<"Further Info: See http://www.postfix.org">>,
                  <<"">>,
                  <<"Site Contact: John Smith, Postmaster">>,
                  <<"Email:        <postmaster@example.com>">>,
                  <<"Telephone:    +1-123-456-7890">>,
                  <<"">>,
                  <<"Please don't send me SPAM here - we don't like it">>]},
                 #{data => <<>>, state => initial, msg_type => reply}},
               smtp_parser:parse(Parser, MultiLineReply)),

  %% Many reply
  Replies = <<"220 mail.example.com ESMTP Postfix\r\n220-mail.example.com ESMTP Postfix\r\n220 welcome!\r\n">>,
  {ok, Reply1, Parser4} = smtp_parser:parse(Parser, Replies),
  ?assertEqual({220, [<<"mail.example.com ESMTP Postfix">>]},
               Reply1),
  ?assertEqual(#{state => initial, msg_type => reply,
                 data => <<"220-mail.example.com ESMTP Postfix\r\n220 welcome!\r\n">>},
               Parser4),

  {ok, Reply2, Parser5}  = smtp_parser:parse(Parser4, <<>>),
  ?assertEqual({220, [<<"mail.example.com ESMTP Postfix">>, <<"welcome!">>]},
               Reply2),
  ?assertEqual(#{data => <<>>, state => initial, msg_type => reply},
               Parser5),

  %% Errors
  ?assertEqual({error, {invalid_line, invalid_syntax}},
               smtp_parser:parse(Parser, <<"\r\n">>)),

  ?assertEqual({error, {invalid_line, invalid_syntax}},
               smtp_parser:parse(Parser, <<"999\r\n">>)),

  ?assertEqual({error, {invalid_line, {invalid_separator, <<"_">>}}},
               smtp_parser:parse(Parser, <<"200_hello\r\n">>)),

  ?assertEqual({error, {invalid_line, code_mismatch}},
               smtp_parser:parse(Parser, <<"220-mail.example.com ESMTP Postfix\r\n200 welcome!\r\n">>)),

  ?assertEqual({error, {invalid_line, {invalid_separator, <<"_">>}}},
               smtp_parser:parse(Parser, <<"220-mail.example.com ESMTP Postfix\r\n220_welcome!\r\n">>)),

  ?assertEqual({error, {invalid_line, {invalid_code, <<"999">>}}},
               smtp_parser:parse(Parser, <<"999 hello\r\n">>)),

  ?assertEqual({error, {invalid_line, {invalid_separator, <<"0">>}}},
               smtp_parser:parse(Parser, <<"2000 hello\r\n">>)),

  ?assertEqual({error, {invalid_line, {invalid_code, <<"AAA">>}}},
               smtp_parser:parse(Parser, <<"AAA hello\r\n">>)).
