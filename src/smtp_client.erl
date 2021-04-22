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

-module(smtp_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

-export_type([options/0, tcp_option/0, tls_option/0, transport/0]).

-type options() :: #{host => uri:host(),
                     port => uri:port_number(),
                     transport => transport(),
                     tcp_options => [tcp_option()],
                     tls_options => [tls_option()],
                     connection_timeout => timeout(),
                     read_timeout => timeout(),
                     log_requests => boolean()}.

-type tcp_option() :: gen_tcp:connect_option().
-type tls_option() :: ssl:tls_client_option().

-type state() :: #{options := options(),
                   transport := transport(),
                   parser := smtp_parser:parser(),
                   socket := inet:socket() | ssl:sslsocket()}.

-type transport() :: tcp | tls.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => [smtp, client]}),
  connect(Options).

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, #{transport := tcp, socket := Socket}) ->
  ?LOG_INFO("closing connection"),
  gen_tcp:close(Socket),
  ok;
terminate(_Reason, #{transport := tls, socket := Socket}) ->
  ?LOG_INFO("closing connection"),
  ssl:close(Socket),
  ok.

handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info({Event, _}, _State) when Event =:= tcp_closed;
                                     Event =:= ssl_closed ->
  ?LOG_INFO("connection closed"),
  exit(normal);

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec connect(options()) -> et_gen_server:init_ret(state()).
connect(Options) ->
  Transport = maps:get(transport, Options, tcp),
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, 25),
  %% Specifications does not recommends any duration for the connection
  %% timeout, as SMTP timeouts are often long durations I set connect timeout
  %% to 1 minute.
  Timeout = maps:get(connection_timeout, Options, 60_000),
  %% The RFC 5321 section 2.3.7 explain that SMTP server command should be
  %% send via the transmission channel in "lines". But as this behaviour can
  %% be altered by a proxy or just not respected by some SMTP server, I
  %% decided to not use the socket option `{packet, line}`.
  DefaultConnectOptions = [{mode, binary}, {active, false}],
  ConnectOptions = DefaultConnectOptions ++ options_connect_options(Options),
  ?LOG_DEBUG("connecting to ~s:~b", [Host, Port]),
  HostAddress = host_address(Host),
  ConnectFun = case Transport of
                 tcp -> fun gen_tcp:connect/4;
                 tls -> fun ssl:connect/4
               end,
  case ConnectFun(HostAddress, Port, ConnectOptions, Timeout) of
    {ok, Socket} ->
      State = #{options => Options,
                transport => Transport,
                socket => Socket,
                parser => smtp_parser:new(reply)},
      {ok, State, {continue, ehlo}};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {stop, normal}
  end.

-spec options_connect_options(options()) -> [Options] when
    Options :: tcp_option() | tls_option().
options_connect_options(Options = #{transport := tls}) ->
  maps:get(tcp_options, Options, []) ++ maps:get(tls_options, Options, []);
options_connect_options(Options) ->
  maps:get(tcp_options, Options, []).

-spec host_address(uri:host()) -> inet:hostname() | inet:socket_address().
host_address(Host) ->
  %% While low level connection functions are perfectly able to connect to an
  %% IP address passed as a string, some features such as ssl peer hostname
  %% verification treat host strings as hostnames even if they represent an IP
  %% address. In the ssl case, they will check for SAN DNS names entries
  %% instead of SAN IP address entries.
  %%
  %% Therefore we check the host string to see if it is an IP address; when
  %% this is the case, we use the inet socket address format (a tuple).
  HostString = binary_to_list(Host),
  case inet:parse_address(HostString) of
    {ok, Address} ->
      Address;
    {error, _} ->
      HostString
  end.

-spec exec(state(), smtp_proto:command(), smtp_proto:code(), timeout()) ->
        smtp_parser:parse_result() | {error, term()}.
exec(#{transport := T, socket := S, parser := P}, Command, Code, Timeout) ->
  case send(T, S, Command) of
    ok ->
      case recv(T, S, Timeout, P) of
        {ok, #{code := Code} = Reply, NewParser} ->
          {ok, Reply, NewParser};
        {ok, Reply, NewParser} ->
          {error, {unexpected_code, Reply, NewParser}};
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, {connection_error, Reason}}
  end.

-spec send(transport(), Socket, binary()) ->
        ok | {error, term()}
          when Socket :: inet:socket() | ssl:sslsocket().
send(Transport, Socket, Packet) ->
  Send = case Transport of
           tcp -> fun gen_tcp:send/2;
           tls -> fun ssl:send/2
         end,
  case Send(Socket, Packet) of
    ok ->
      ok;
    {error, Reason} ->
      ?LOG_ERROR("write packet failed: ~p", [Reason]),
      {error, Reason}
  end.

-spec recv(transport(), Socket, timeout(), smtp_parser:parser()) ->
        {ok, smtp_parser:msg(), smtp_parser:parser()} | {error, term()}
          when Socket :: inet:socket() | ssl:sslsocket().
recv(Transport, Socket, Timeout, Parser) ->
  Recv = case Transport of
           tcp -> fun gen_tcp:recv/3;
           tls -> fun ssl:recv/3
         end,
  case Recv(Socket, 0, Timeout) of
    {ok, Packet} ->
      case smtp_parser:parse(Parser, Packet) of
        {ok, Reply, NewParser} ->
          {ok, Reply, NewParser};
        {more, NewParser} ->
          recv(Transport, Socket, Timeout, NewParser);
        {error, Reason} ->
          ?LOG_ERROR("parse smtp reply failed: ~p", [Reason]),
          {error, Reason}
        end;
    {error, Reason} ->
      ?LOG_ERROR("receive packet failed: ~p", [Reason]),
      {error, Reason}
    end.
