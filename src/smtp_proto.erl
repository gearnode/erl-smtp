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
         encode_starttls_cmd/0, encode_auth_cmd/1, encode_empty_cmd/0,
         encode_mail_from_cmd/1]).

-export([decode_ehlo_reply/1, decode_helo_reply/1, decode_auth_reply/1]).

-export_type([command/0]).

-export_type([ehlo_reply/0, helo_reply/0, auth_reply/0]).

-type command() :: binary().

-type ehlo_reply() :: #{domain := binary(), info := binary(),
                        extensions := [{binary(), term()}]}.

-type helo_reply() :: #{domain := binary(), info := binary()}.

-type auth_reply() :: #{challenge := binary()}.

-spec encode_ehlo_cmd(uri:host()) -> command().
encode_ehlo_cmd(DomainName) ->
  command(<<"EHLO">>, DomainName).

-spec encode_helo_cmd(uri:host()) -> command().
encode_helo_cmd(DomainName) ->
  command(<<"HELO">>, DomainName).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.5
-spec encode_rset_cmd() -> command().
encode_rset_cmd() ->
  command(<<"RSET">>).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.6
-spec encode_vrfy_cmd(binary()) -> command().
encode_vrfy_cmd(Id) ->
  command(<<"VRFY">>, Id).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.7
-spec encode_expn_cmd(binary()) -> command().
encode_expn_cmd(Id) ->
  command(<<"EXPN">>, Id).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.8
-spec encode_help_cmd() -> command().
encode_help_cmd() ->
  command(<<"HELP">>).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.8
-spec encode_help_cmd(binary()) -> command().
encode_help_cmd(Arg) ->
  command(<<"HELP">>, Arg).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.9
-spec encode_noop_cmd() -> command().
encode_noop_cmd() ->
  command(<<"NOOP">>).

%% https://tools.ietf.org/html/rfc5321#section-4.1.1.10
-spec encode_quit_cmd() -> command().
encode_quit_cmd() ->
  command(<<"QUIT">>).

%% https://tools.ietf.org/html/rfc3207
-spec encode_starttls_cmd() -> command().
encode_starttls_cmd() ->
  command(<<"STARTTLS">>).

%% https://tools.ietf.org/html/rfc4616
-spec encode_auth_cmd(binary()) -> command().
encode_auth_cmd(Mechanism) ->
  command(<<"AUTH">>, Mechanism).

-spec encode_empty_cmd() -> command().
encode_empty_cmd() ->
  <<$\r, $\n>>.

-spec encode_mail_from_cmd(binary()) -> command().
encode_mail_from_cmd(Email) ->
  command(<<"MAIL FROM:">>, <<$<, Email/binary, $>>>).

-spec command(binary()) -> command().
command(Keyword) ->
  <<Keyword/binary, $\r, $\n>>.

-spec command(binary(), binary()) -> command().
command(Keyword, Arg) ->
  <<Keyword/binary, $\s, Arg/binary, $\r, $\n>>.

-spec decode_ehlo_reply(smtp_reply:lines()) -> ehlo_reply().
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
decode_ehlo_extensions([<<"STARTTLS">> | Rest], Acc) ->
  decode_ehlo_extensions(Rest,[{<<"STARTTLS">>, true} | Acc]);
decode_ehlo_extensions([<<"AUTH", $\s, Bin/binary>> | Rest], Acc) ->
  AvailableMechanisms = binary:split(Bin, <<$\s>>, [global]),
  decode_ehlo_extensions(Rest, [{<<"AUTH">>, AvailableMechanisms} | Acc]);
decode_ehlo_extensions([Bin | Rest], Acc) ->
  decode_ehlo_extensions(Rest,[{Bin, false} | Acc]).

-spec decode_helo_reply(smtp_reply:lines()) -> helo_reply().
decode_helo_reply([]) ->
  #{info => <<>>};
decode_helo_reply([Bin | _]) ->
  {Domain, Info} =
    case binary:split(Bin, <<$\s>>) of
      [V1, V2] -> {V1, V2};
      [V1] -> {V1, <<>>}
    end,
  #{domain => Domain, info => Info}.

-spec decode_auth_reply(smtp_reply:lines()) ->
        {ok, auth_reply()} | {error, term()}.
decode_auth_reply([Bin | _]) ->
  case b64:decode(Bin) of
    {ok, Challenge} ->
      {ok, #{challenge => Challenge}};
    {error, Reason} ->
      {error, Reason}
  end.
