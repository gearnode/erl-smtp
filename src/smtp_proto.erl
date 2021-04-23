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

-module(smtp_proto).

-export([encode_ehlo_cmd/1, encode_helo_cmd/1, encode_help_cmd/0,
         encode_help_cmd/1, encode_noop_cmd/0, encode_quit_cmd/0,
         encode_rset_cmd/0, encode_vrfy_cmd/1, encode_expn_cmd/1,
         decode_ehlo_reply/1]).

-export_type([command/0]).

-type command() :: binary().

-spec encode_ehlo_cmd(uri:host()) -> command().
encode_ehlo_cmd(DomainName) ->
  command(<<"EHLO">>, DomainName).

-spec encode_helo_cmd(uri:host()) -> command().
encode_helo_cmd(DomainName) ->
  command(<<"HELO">>, DomainName).

-spec encode_rset_cmd() -> command().
encode_rset_cmd() ->
  command(<<"RSET">>).

-spec encode_vrfy_cmd(binary()) -> command().
encode_vrfy_cmd(Id) ->
  command(<<"VRFY">>, Id).

-spec encode_expn_cmd(binary()) -> command().
encode_expn_cmd(Id) ->
  command(<<"EXPN">>, Id).

-spec encode_help_cmd() -> command().
encode_help_cmd() ->
  command(<<"HELP">>).

-spec encode_help_cmd(binary()) -> command().
encode_help_cmd(Arg) ->
  command(<<"HELP">>, Arg).

-spec encode_noop_cmd() -> command().
encode_noop_cmd() ->
  command(<<"NOOP">>).

-spec encode_quit_cmd() -> command().
encode_quit_cmd() ->
  command(<<"QUIT">>).

-spec command(binary()) -> command().
command(Keyword) ->
  <<Keyword/binary, $\r, $\n>>.

-spec command(binary(), binary()) -> command().
command(Keyword, Arg) ->
  <<Keyword/binary, $\s, Arg/binary, $\r, $\n>>.

-spec decode_ehlo_reply(smtp_reply:lines()) ->
        term().
decode_ehlo_reply([Bin | Rest]) ->
  {Domain, Info} =
    case binary:split(Bin, <<$\s>>) of
      [V1, V2] -> {V1, V2};
      [V1] -> {V1, <<>>}
    end,
  Extensions = decode_ehlo_extensions(Rest, []),
  #{domain => Domain, info => Info,
    extensions => Extensions}.

decode_ehlo_extensions([], Acc) ->
  lists:reverse(Acc);
decode_ehlo_extensions([<<"8BITMIME">> | Rest], Acc) ->
  decode_ehlo_extensions(Rest, [{<<"8BITMIME">>, true} | Acc]);
decode_ehlo_extensions([<<"ENHANCEDSTATUSCODES">> | Rest], Acc) ->
  decode_ehlo_extensions(Rest, [{<<"ENHANCEDSTATUSCODES">>, true} | Acc]);
decode_ehlo_extensions([<<"SIZE", $\s, Size/binary>> | Rest], Acc) ->
  decode_ehlo_extensions(Rest, [{<<"SIZE">>, Size} | Acc]);
decode_ehlo_extensions([<<"HELP">> | Rest], Acc) ->
  decode_ehlo_extensions(Rest,[{<<"HELP">>, true} | Acc]);
decode_ehlo_extensions([<<"DSN">> | Rest], Acc) ->
  decode_ehlo_extensions(Rest,[{<<"DSN">>, true} | Acc]);
decode_ehlo_extensions([Bin | Rest], Acc) ->
  decode_ehlo_extensions(Rest,[{Bin, false} | Acc]).
