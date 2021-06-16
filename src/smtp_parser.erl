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

-module(smtp_parser).

-export([new/1, parse/2]).

-export_type([msg_type/0, msg/0, parser/0, state/0,
              parse_result/0, parse_error_reason/0]).

-type msg_type() :: command | reply.

-type msg() :: smtp_reply:reply().

-type parser() :: #{data := binary(),
                    state := state(),
                    msg_type := msg_type(),
                    msg => msg()}.

-type state() :: initial
               | reply_line
               | final.

-type parse_result() :: {ok, msg(), parser()}
                      | {more, parser()}
                      | {error, parse_error_reason()}.

-type parse_error_reason() ::
        {invalid_line, invalid_syntax}
      | {invalid_line, {invalid_separator, binary()}}
      | {invalid_line, {invalid_code, binary()}}
      | {invalid_line, {invalid_code, code_mismatch}}.

-spec new(msg_type()) -> parser().
new(Type) ->
  #{data => <<>>,
    state => initial,
    msg_type => Type}.

-spec parse(parser(), binary()) -> parse_result().
parse(Parser = #{data := Data}, NewData) ->
  try
    parse(Parser#{data => <<Data/binary, NewData/binary>>})
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse(parser()) -> parse_result().
parse(Parser = #{msg_type := reply, state := initial}) ->
  parse(Parser#{state => reply_line});

parse(Parser = #{data := Data, msg_type := reply, state := reply_line}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      case maps:is_key(msg, Parser) of
        true ->
          parse_continuation_line(Parser, Line, Rest);
        false ->
          parse_first_line(Parser, Line, Rest)
      end;
    _ ->
      {more, Parser}
  end;

parse(Parser = #{state := final, msg := {Code, Lines}}) ->
  Parser2 = maps:remove(msg, Parser),
  Msg = {Code, lists:reverse(Lines)},
  {ok, Msg, Parser2#{state => initial}}.

-spec parse_first_line(parser(), binary(), binary()) ->
        parse_result().
parse_first_line(Parser, Line, Rest) ->
  case parse_reply_line(Line) of
    {Code, Sep, NewLine} ->
      Parser2 = Parser#{data => Rest, msg => {Code, [NewLine]}},

      case Sep of
        sp -> parse(Parser2#{state => final});
        minus -> parse(Parser2)
      end;
    {error, Reason} ->
      throw({error, {invalid_line, Reason}})
  end.

-spec parse_continuation_line(parser(), binary(), binary()) ->
        parse_result().
parse_continuation_line(Parser = #{msg := {Code, Lines}}, Line, Rest) ->
  case parse_reply_line(Line) of
    {Code, Sep, NewLine} ->
      Parser2 = Parser#{data => Rest, msg => {Code, [NewLine | Lines]}},

      case Sep of
        sp -> parse(Parser2#{state => final});
        minus -> parse(Parser2)
      end;
    {_, _, _} ->
      throw({error, {invalid_line, code_mismatch}});
    {error, Reason} ->
      throw({error, {invalid_line, Reason}})
  end.

-spec parse_reply_line(binary()) ->
        {smtp_reply:code(), smtp_reply:separator(), smtp_reply:line()} |
        {error, Reason}
          when Reason :: invalid_syntax
                       | {invalid_separator, binary()}
                       | {invalid_code, binary()}.
parse_reply_line(<<Bin1:3/binary, Bin2:1/binary, Rest/binary>>) ->
  case parse_code(Bin1) of
    {ok, Code} ->
      case parse_separator(Bin2) of
        {error, Reason} ->
          {error, Reason};
        {ok, Separator} ->
          {Code, Separator, Rest}
      end;
    {error, Reason} ->
      {error, Reason}
  end;
parse_reply_line(_) ->
  {error, invalid_syntax}.

-spec parse_separator(binary()) ->
        {ok, smtp_reply:separator()} | {error, {invalid_separator, binary()}}.
parse_separator(<<" ">>) ->
  {ok, sp};
parse_separator(<<"-">>) ->
  {ok, minus};
parse_separator(Bin) ->
  {error, {invalid_separator, Bin}}.

-spec parse_code(binary()) ->
        {ok, smtp_reply:code()} | {error, {invalid_code, binary()}}.
parse_code(<<A:8, B:8, C:8>> = Bin) when A >= $2, A =< $5,
                                         B >= $0, B =< $5,
                                         C >= $0, C =< $9 ->
  {ok, binary_to_integer(Bin)};
parse_code(Bin) ->
  {error, {invalid_code, Bin}}.
