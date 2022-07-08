%% Copyright (c) 2022 Bryan Frimin <bryan@frimin.fr>.
%% Copyright (c) 2021-2022 Exograd SAS.
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

encode_ehlo_cmd_test_() ->
  [?_assertEqual(<<"EHLO mail.example.com\r\n">>,
                 smtp_proto:encode_ehlo_cmd(<<"mail.example.com">>)),
   ?_assertEqual(<<"EHLO localhost\r\n">>,
                 smtp_proto:encode_ehlo_cmd(<<"localhost">>))].

encode_helo_cmd_test_() ->
  [?_assertEqual(<<"HELO mail.example.com\r\n">>,
                 smtp_proto:encode_helo_cmd(<<"mail.example.com">>)),
   ?_assertEqual(<<"HELO localhost\r\n">>,
                 smtp_proto:encode_helo_cmd(<<"localhost">>))].

encode_rset_cmd_test_() ->
  [?_assertEqual(<<"RSET\r\n">>,
                 smtp_proto:encode_rset_cmd())].

encode_vrfy_cmd_test_() ->
  [?_assertEqual(<<"VRFY contact@example.com\r\n">>,
                 smtp_proto:encode_vrfy_cmd(<<"contact@example.com">>))].

encode_expn_cmd_test_() ->
  [?_assertEqual(<<"EXPN ml@example.com\r\n">>,
                 smtp_proto:encode_expn_cmd(<<"ml@example.com">>))].

encode_help_cmd_test_() ->
  [?_assertEqual(<<"HELP\r\n">>,
                 smtp_proto:encode_help_cmd()),
   ?_assertEqual(<<"HELP EXPN\r\n">>,
                 smtp_proto:encode_help_cmd(<<"EXPN">>))].

encode_noop_cmd_test_() ->
  [?_assertEqual(<<"NOOP\r\n">>,
                 smtp_proto:encode_noop_cmd())].

encode_quit_cmd_test_() ->
  [?_assertEqual(<<"QUIT\r\n">>,
                 smtp_proto:encode_quit_cmd())].

decode_ehlo_reply_test() ->
  Reply =
    [<<"d01c7054a707 Hello basile.localdomain [172.26.0.1], pleased to meet you">>,
     <<"8BITMIME">>,<<"ENHANCEDSTATUSCODES">>,<<"SIZE 36700160">>,<<"DSN">>,
     <<"HELP">>, <<"UNKOWNEXT">>],
  ?assertEqual(#{domain => <<"d01c7054a707">>,
                 extensions =>
                   [{<<"8BITMIME">>,true},
                    {<<"ENHANCEDSTATUSCODES">>,true},
                    {<<"SIZE">>,<<"36700160">>},
                    {<<"DSN">>,true},
                    {<<"HELP">>,true},
                    {<<"UNKOWNEXT">>, false}],
                 info =>
                   <<"Hello basile.localdomain [172.26.0.1], pleased to meet you">>},
               smtp_proto:decode_ehlo_reply(Reply)),

  ?assertEqual(#{domain => <<"d01c7054a707">>,
                 extensions => [],
                 info => <<>>},
               smtp_proto:decode_ehlo_reply([<<"d01c7054a707">>])).

decode_helo_reply_test() ->
  ?assertEqual(#{info => <<>>}, smtp_proto:decode_helo_reply([])),
  Reply =
    [<<"d01c7054a707 Hello basile.localdomain [172.26.0.1], pleased to meet you">>],
  ?assertEqual(#{domain => <<"d01c7054a707">>,
                 info =>
                   <<"Hello basile.localdomain [172.26.0.1], pleased to meet you">>},
               smtp_proto:decode_helo_reply(Reply)).
