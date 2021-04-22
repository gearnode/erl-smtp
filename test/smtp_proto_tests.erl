%% Copyright (c) 2021 Bryan Frimin <bryan@frimin.fr>.
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

-module(smtp_proto_tests).

-include_lib("eunit/include/eunit.hrl").

encode_ehlo_test_() ->
  [?_assertEqual(<<"EHLO mail.example.com\r\n">>,
                 smtp_proto:encode_ehlo(<<"mail.example.com">>)),
   ?_assertEqual(<<"EHLO localhost\r\n">>,
                 smtp_proto:encode_ehlo(<<"localhost">>))].

encode_helo_test_() ->
  [?_assertEqual(<<"HELO mail.example.com\r\n">>,
                 smtp_proto:encode_helo(<<"mail.example.com">>)),
   ?_assertEqual(<<"HELO localhost\r\n">>,
                 smtp_proto:encode_helo(<<"localhost">>))].

encode_rset_test_() ->
  [?_assertEqual(<<"RSET\r\n">>,
                 smtp_proto:encode_rset())].

encode_vrfy_test_() ->
  [?_assertEqual(<<"VRFY contact@example.com\r\n">>,
                 smtp_proto:encode_vrfy(<<"contact@example.com">>))].

encode_expn_test_() ->
  [?_assertEqual(<<"EXPN ml@example.com\r\n">>,
                 smtp_proto:encode_expn(<<"ml@example.com">>))].

encode_help_test_() ->
  [?_assertEqual(<<"HELP\r\n">>,
                 smtp_proto:encode_help())].

encode_noop_test_() ->
  [?_assertEqual(<<"NOOP\r\n">>,
                 smtp_proto:encode_noop())].

encode_quit_test_() ->
  [?_assertEqual(<<"QUIT\r\n">>,
                 smtp_proto:encode_quit())].

parse_reply_line_test_() ->
  [?_assertEqual({220, sp, <<"mail.example.com Postfix">>},
                 smtp_proto:parse_reply_line(<<"220 mail.example.com Postfix">>)),
   ?_assertEqual({220, sp, <<>>},
                 smtp_proto:parse_reply_line(<<"220 ">>)),
   ?_assertEqual({220, minus, <<"d01c7054a707 ESMTP OpenSMTPD">>},
                 smtp_proto:parse_reply_line(<<"220-d01c7054a707 ESMTP OpenSMTPD">>)),
   ?_assertEqual({error, invalid_code},
                 smtp_proto:parse_reply_line(<<"999-hello">>)),
   ?_assertEqual({error, invalid_separator},
                 smtp_proto:parse_reply_line(<<"2000-hello">>)),
   ?_assertEqual({error, invalid_line},
                 smtp_proto:parse_reply_line(<<>>)),
   ?_assertEqual({error, invalid_code},
                 smtp_proto:parse_reply_line(<<"AAA hello">>))].
