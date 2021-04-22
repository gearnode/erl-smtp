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
         parse_reply/1]).

-export_type([code/0, separator/0, text/0, command/0]).

-type code() :: 001..599.
-type separator() :: minus | sp.
-type text() :: binary().

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

-spec parse_reply(binary()) ->
        {code(), separator(), text()} | {error, term()}.
parse_reply(<<Code0:3/binary, Separator0:1/binary, Rest/binary>>) ->
  case parse_code(Code0) of
    {ok, Code} ->
      case parse_separator(Separator0) of
        {error, Reason} ->
          {error, Reason};
        {ok, Separator} ->
          {Code, Separator, Rest}
      end;
    {error, Reason} ->
      {error, Reason}
  end;
parse_reply(_) ->
  {error, invalid_line}.

-spec parse_separator(binary()) ->
        {ok, separator()} | {error, term()}.
parse_separator(<<" ">>) ->
  {ok, sp};
parse_separator(<<"-">>) ->
  {ok, minus};
parse_separator(_) ->
  {error, invalid_separator}.

-spec parse_code(binary()) ->
        {ok, pos_integer()} | {error, term()}.
parse_code(Value) ->
  try
    binary_to_integer(Value)
  of
    N when N > 0, N < 600 ->
      {ok, N};
    _ ->
      {error, invalid_code}
  catch
    error:_ ->
      {error, invalid_code}
  end.
