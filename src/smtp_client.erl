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
         handle_call/3, handle_cast/2, handle_info/2]).

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
                   socket := inet:socket() | ssl:sslsocket()}.

-type transport() :: tcp | tls.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => log_domain()}),
  case connect(Options) of
    {ok, State} ->
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, #{transport := tcp, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  gen_tcp:close(Socket),
  ok;
terminate(_Reason, #{transport := tls, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  ssl:close(Socket),
  ok.

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
  ?LOG_DEBUG("connection closed"),
  exit(normal);

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec connect(options()) -> {ok, state()} | {error, term()}.
connect(Options) ->
  Transport = maps:get(transport, Options, tcp),
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, 25),
  Timeout = maps:get(connection_timeout, Options, 5000),

  %% Enable line mode as defined by the RFC 5321 section 2.3.7
  RequiredConnectOptions = [{mode, binary}, {active, false}, {packet, line}],
  ConnectOptions = RequiredConnectOptions ++ options_connect_options(Options),
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
                socket => Socket},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, Reason}
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

-spec log_domain() -> [atom()].
log_domain() ->
  [smtp, client].
