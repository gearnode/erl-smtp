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

-export([parse_reply_line/1]).

-export_type([code/0, separator/0, text/0]).

-type code() :: 001..599.
-type separator() :: minus | sp.
-type text() :: binary().

-spec parse_reply_line(binary()) ->
        {code(), separator(), text()} | {error, term()}.
parse_reply_line(<<Code0:3/binary, Separator0:1/binary, Rest/binary>>) ->
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
parse_reply_line(_) ->
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
parse_code(Value) when byte_size(Value) =:= 3 ->
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
  end;
parse_code(_) ->
  {error, invalid_format}.
