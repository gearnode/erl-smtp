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
                 smtp_proto:encode_ehlo_cmd(<<"mail.example.com">>)),
   ?_assertEqual(<<"EHLO localhost\r\n">>,
                 smtp_proto:encode_ehlo_cmd(<<"localhost">>))].

encode_helo_test_() ->
  [?_assertEqual(<<"HELO mail.example.com\r\n">>,
                 smtp_proto:encode_helo_cmd(<<"mail.example.com">>)),
   ?_assertEqual(<<"HELO localhost\r\n">>,
                 smtp_proto:encode_helo_cmd(<<"localhost">>))].

encode_rset_test_() ->
  [?_assertEqual(<<"RSET\r\n">>,
                 smtp_proto:encode_rset_cmd())].

encode_vrfy_test_() ->
  [?_assertEqual(<<"VRFY contact@example.com\r\n">>,
                 smtp_proto:encode_vrfy_cmd(<<"contact@example.com">>))].

encode_expn_test_() ->
  [?_assertEqual(<<"EXPN ml@example.com\r\n">>,
                 smtp_proto:encode_expn_cmd(<<"ml@example.com">>))].

encode_help_test_() ->
  [?_assertEqual(<<"HELP\r\n">>,
                 smtp_proto:encode_help_cmd()),
   ?_assertEqual(<<"HELP EXPN\r\n">>,
                 smtp_proto:encode_help_cmd(<<"EXPN">>))].

encode_noop_test_() ->
  [?_assertEqual(<<"NOOP\r\n">>,
                 smtp_proto:encode_noop_cmd())].

encode_quit_test_() ->
  [?_assertEqual(<<"QUIT\r\n">>,
                 smtp_proto:encode_quit_cmd())].

parse_reply_test_() ->
  [?_assertEqual({220, sp, <<"mail.example.com Postfix">>},
                 smtp_proto:parse_reply(<<"220 mail.example.com Postfix">>)),
   ?_assertEqual({220, sp, <<>>},
                 smtp_proto:parse_reply(<<"220 ">>)),
   ?_assertEqual({220, minus, <<"d01c7054a707 ESMTP OpenSMTPD">>},
                 smtp_proto:parse_reply(<<"220-d01c7054a707 ESMTP OpenSMTPD">>)),
   ?_assertEqual({error, invalid_code},
                 smtp_proto:parse_reply(<<"999-hello">>)),
   ?_assertEqual({error, invalid_separator},
                 smtp_proto:parse_reply(<<"2000-hello">>)),
   ?_assertEqual({error, invalid_line},
                 smtp_proto:parse_reply(<<>>)),
   ?_assertEqual({error, invalid_code},
                 smtp_proto:parse_reply(<<"AAA hello">>))].
