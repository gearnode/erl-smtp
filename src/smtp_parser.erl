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

-type msg() :: #{code => smtp_proto:code(),
                 text => [smtp_proto:text()]}.

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

-type parse_error_reason() :: term().

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

parse(Parser = #{state := final, msg := Msg0}) ->
  Parser2 = maps:remove(msg, Parser),
  Msg = Msg0#{text => lists:reverse(maps:get(text, Msg0))},
  {ok, Msg, Parser2#{state => initial}}.

-spec parse_first_line(parser(), binary(), binary()) ->
        parse_result().
parse_first_line(Parser, Line, Rest) ->
  case smtp_proto:parse_reply_line(Line) of
    {Code, Sep, LineText} ->
      Msg = #{code => Code, text => [LineText]},
      Parser2 = Parser#{data => Rest, msg => Msg},

      case Sep of
        sp -> parse(Parser2#{state => final});
        minus -> parse(Parser2)
      end;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec parse_continuation_line(parser(), binary(), binary()) ->
        parse_result().
parse_continuation_line(Parser = #{msg := Msg}, Line, Rest) ->
  Code = maps:get(code, Msg),
  Text = maps:get(text, Msg),

  case smtp_proto:parse_reply_line(Line) of
    {Code, Sep, LineText} ->
      Msg2 = Msg#{text => [LineText | Text]},
      Parser2 = Parser#{data => Rest, msg => Msg2},

      case Sep of
        sp -> parse(Parser2#{state => final});
        minus -> parse(Parser2)
      end;
    {_, _, _} ->
      throw({error, invalid_reply});
    {error, Reason} ->
      throw({error, Reason})
  end.
