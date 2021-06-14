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

-module(smtpc).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, init/1, terminate/2,
         handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

-export([quit/1, noop/1, sendmail/3, sendmail/4]).

-export_type([options/0, tcp_option/0, tls_option/0, transport/0,
              command_timeout/0, starttls_policy/0,
              authentication/0, mechanism_name/0, mechanism_parameters/0]).

-type options() :: #{host => uri:host(),
                     port => uri:port_number(),
                     transport => transport(),
                     starttls => starttls_policy(),
                     tcp_options => [tcp_option()],
                     tls_options => [tls_option()],
                     authentication => authentication(),
                     connection_timeout => timeout(),
                     read_timeouts => command_timeout(),
                     log_requests => boolean()}.

-type tcp_option() :: gen_tcp:connect_option().
-type tls_option() :: ssl:tls_client_option().

-type server_info() :: smtp_proto:ehlo_reply()
                     | smtp_proto:helo_reply().

-type state() :: #{options := options(),
                   transport := transport(),
                   parser := smtp_parser:parser(),
                   socket := inet:socket() | ssl:sslsocket(),
                   server_info => server_info()}.

-type transport() :: tcp | tls.

-type starttls_policy() :: disabled | required | best_effort.

-type authentication() :: {mechanism_name(), mechanism_parameters()}.

-type mechanism_name() :: binary().
-type mechanism_parameters() :: #{username := binary(), password := binary()}.

-type command_timeout() :: #{binary() => timeout()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec quit(et_gen_server:ref()) -> ok | {error, term()}.
quit(Ref) ->
  gen_server:call(Ref, quit, infinity).

-spec sendmail(et_gen_server:ref(), binary(), imf:message()) ->
        ok | {error, term()}.
sendmail(Ref, From, Mail) ->
  To = imf:recipient_addresses(Mail),
  Data = imf:encode(Mail),
  gen_server:call(Ref, {sendmail, From, To, Data}, infinity).

-spec sendmail(et_gen_server:ref(), binary(), binary(), binary()) ->
        ok | {error, term()}.
sendmail(Ref, From, To, Data) ->
  gen_server:call(Ref, {sendmail, From, To, Data}, infinity).

-spec noop(et_gen_server:ref()) -> ok | {error, term()}.
noop(Ref) ->
  gen_server:call(Ref, noop, infinity).

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

-spec handle_continue(term(), state()) ->
        et_gen_server:handle_continue_ret(state()).
handle_continue(ehlo, State) ->
  greeting_message(State);

handle_continue(Msg, State) ->
  ?LOG_WARNING("unhandled call ~p", [Msg]),
  {noreply, State}.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call(quit, _, State) ->
  case quit_2(State) of
    {ok, State2} ->
      {stop, normal, ok, State2};
    {error, Reason, State2} ->
      {stop, normal, {error, Reason}, State2}
  end;

handle_call(noop, _, State) ->
  case noop_2(State) of
    {ok, State2} ->
      {reply, ok, State2};
    {error, Reason, State2} ->
      {reply, {error, Reason}, State2}
  end;

handle_call({sendmail, From, To, Data}, _, State) ->
  case sendmail_2(From, To, Data, State) of
    {ok, State2} ->
      {reply, ok, State2};
    {error, Reason, State2} ->
      {reply, {error, Reason}, State2}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

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

-spec greeting_message(state()) -> et_gen_server:handle_continue_ret(state()).
greeting_message(#{transport := T, socket := S, parser := P} = State) ->
  Timeout = get_read_timeout_option(State, <<"INITIAL">>, 60_000),
  case recv(T, S, Timeout, P) of
    {ok, #{code := 220}, NewParser} ->
      ehlo(State#{parser => NewParser});
    {ok, #{code := Code, lines := [Line|_]}, NewParser} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec ehlo(state()) -> et_gen_server:handle_continue_ret(state()).
ehlo(State) ->
  Cmd = smtp_proto:encode_ehlo_cmd(smtp:fqdn()),
  Timeout = get_read_timeout_option(State, <<"EHLO">>, 60_000),
  case exec(State, Cmd, 250, Timeout) of
    {ok, #{lines := Lines}, NewParser} ->
      Reply = smtp_proto:decode_ehlo_reply(Lines),
      NewState = State#{server_info => Reply, parser => NewParser},
      maybe_starttls(NewState);
    {error, {unexpected_code, _, NewParser}} ->
      helo(State#{parser => NewParser});
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec helo(state()) -> et_gen_server:handle_continue_ret(state()).
helo(State) ->
  Cmd = smtp_proto:encode_helo_cmd(smtp:fqdn()),
  Timeout = get_read_timeout_option(State, <<"HELO">>, 60_000),
  case exec(State, Cmd, 250, Timeout) of
    {ok, #{lines := Lines}, NewParser} ->
      Reply = smtp_proto:decode_helo_reply(Lines),
      maybe_starttls(State#{parser => NewParser, server_info => Reply});
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec maybe_starttls(state()) -> et_gen_server:handle_continue_ret(state()).
maybe_starttls(#{transport := tls} = State) ->
  maybe_auth(State);
maybe_starttls(State) ->
  case get_starttls_policy_option(State) of
    disabled ->
      maybe_auth(State);
    required ->
      starttls(State);
    best_effort ->
      starttls(State)
  end.

-spec starttls(state()) -> et_gen_server:handle_continue_ret(state()).
starttls(State) ->
  Cmd = smtp_proto:encode_starttls_cmd(),
  Timeout = get_read_timeout_option(State, <<"STARTTLS">>, 60_000),
  case exec(State, Cmd, 220, Timeout) of
    {ok, _, NewParser} ->
      ssl_handshake(State#{parser => NewParser});
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      case get_starttls_policy_option(State) of
        required ->
          {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
        best_effort ->
          {noreply, State#{parser => NewParser}}
      end;
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec ssl_handshake(state()) -> et_gen_server:handle_continue_ret(state()).
ssl_handshake(#{options := Options, socket := Socket} = State) ->
  TLSOptions = maps:get(tls_options, Options, []),
  case ssl:connect(Socket, TLSOptions) of
    {ok, SSLSocket} ->
      ehlo(State#{transport => tls, socket => SSLSocket});
    {error, Reason} ->
      {stop, {connection_error, Reason}, State}
  end.

-spec maybe_auth(state()) -> et_gen_server:handle_continue_ret(state()).
maybe_auth(State) ->
  case get_authentication_option(State) of
    {Mechanism, MechanismOptions} when
        Mechanism =:= <<"PLAIN">>;
        Mechanism =:= <<"LOGIN">>;
        Mechanism =:= <<"CRAM-MD5">>;
        Mechanism =:= <<"XOAUTH2">> ->
      auth(Mechanism, MechanismOptions, State);
    {Mechanism, _} ->
      {stop, {unsupported_sasl_mechanism, client, Mechanism}, State};
    error ->
      {noreply, State}
  end.

-spec auth(mechanism_name(), mechanism_parameters(), state()) ->
        et_gen_server:handle_continue_ret(state()).
auth(Mechanism, MechanismOptions, State) ->
  Timeout = get_read_timeout_option(State, <<"AUTH">>, 60_000),
  Cmd = smtp_proto:encode_auth_cmd(Mechanism),
  case exec(State, Cmd, 334, Timeout) of
    {ok, #{lines := Lines}, NewParser} ->
      case smtp_proto:decode_auth_reply(Lines) of
        {ok, #{challenge := Challenge}} ->
          auth(Mechanism, MechanismOptions, Challenge,
               State#{parser => NewParser});
        {error, Reason} ->
          {stop, Reason, State#{parser => NewParser}}
      end;
    {error, {unexpected_code, _, NewParser}} ->
      {stop,
       {unsupported_sasl_mechanism, server, Mechanism},
       State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec auth(mechanism_name(), mechanism_parameters(), binary(), state()) ->
        et_gen_server:handle_continue_ret(state()).
auth(<<"PLAIN">>, #{username := Username, password := Password}, _, State) ->
  Timeout = get_read_timeout_option(State, <<"AUTH">>, 60_000),
  Msg = smtp_sasl:encode_plain(Username, Password),
  case exec(State, Msg, 235, Timeout) of
    {ok, _, NewParser} ->
      {noreply, State#{parser => NewParser}};
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end;
auth(<<"LOGIN">>, #{username := Username, password := Password}, _, State) ->
  Timeout = get_read_timeout_option(State, <<"AUTH">>, 60_000),
  {Msg1, Ms2} = smtp_sasl:encode_login(Username, Password),
  case exec(State, Msg1, 334, Timeout) of
    {ok, _, NewParser} ->
      State2 = State#{parser => NewParser},
      case exec(State2, Ms2, 235, Timeout) of
        {ok, _, NewParser2} ->
          {nereply, State2#{parser => NewParser2}};
        {error,
         {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser2}} ->
          {stop, {unexpected_code, Code, Line}, State2#{parser => NewParser2}};
        {error, Reason} ->
          {stop, Reason, State2}
      end;
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end;
auth(<<"CRAM-MD5">>, #{username := U, password := P}, Challenge, State) ->
  Timeout = get_read_timeout_option(State, <<"AUTH">>, 60_000),
  Msg = smtp_sasl:encode_cram_md5(U, P, Challenge),
  case exec(State, Msg, 235, Timeout) of
    {ok, _, NewParser} ->
      {noreply, State#{parser => NewParser}};
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end;
auth(<<"XOAUTH2">>, #{username := Username, password := Password}, _, State) ->
  Timeout = get_read_timeout_option(State, <<"AUTH">>, 60_000),
  Msg = smtp_sasl:encode_xoauth2(Username, Password),
  case exec(State, Msg, 235, Timeout) of
    {ok, _, NewParser} ->
      {noreply, State#{parser => NewParser}};
    {error, {unexpected_code, #{code := 334}, NewParser}} ->
      State2 = State#{parser => NewParser},
      Msg2 = smtp_proto:encode_empty_cmd(),
      case exec(State2, Msg2, 535, Timeout) of
        {ok, #{code := Code, lines := [Line|_]}, NewParser2} ->
          {stop, {unexpected_code, Code, Line}, State2#{parser => NewParser2}};
        {error,
         {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser2}} ->
          {stop, {unexpected_code, Code, Line}, State2#{parser => NewParser2}};
        {error, Reason} ->
          {stop, Reason, State}
      end;
    {error,
     {unexpected_code, #{code := Code, lines := [Line|_]}, NewParser}} ->
      {stop, {unexpected_code, Code, Line}, State#{parser => NewParser}};
    {error, Reason} ->
      {stop, Reason, State}
  end.

-spec quit_2(state()) -> {ok, state()} | {error, term(), state()}.
quit_2(State) ->
  Timeout = get_read_timeout_option(State, <<"QUIT">>, 60_000),
  Cmd = smtp_proto:encode_quit_cmd(),
  case exec(State, Cmd, 221, Timeout) of
    {ok, _, Parser} ->
      {ok, State#{parser => Parser}};
    {error, {unexpected_code, #{code := Code, lines := [Line|_]}, Parser}} ->
      {error, {unexpected_code, Code, Line}, State#{parser => Parser}};
    {error, Reason} ->
      {error, Reason, State}
  end.

-spec noop_2(state()) -> {ok, state()} | {error, term(), state()}.
noop_2(State) ->
  Timeout = get_read_timeout_option(State, <<"NOOP">>, 60_000),
  Cmd = smtp_proto:encode_noop_cmd(),
  case exec(State, Cmd, 250, Timeout) of
    {ok, _, Parser} ->
      {ok, State#{parser => Parser}};
    {error, {unexpected_code, #{code := Code, lines := Lines}, Parser}} ->
      {error,
       {unexpected_code, Code, list_to_binary(Lines)},
       State#{parser => Parser}};
    {error, Reason} ->
      {error, Reason, State}
  end.

-spec sendmail_2(binary(), binary(), binary() | [binary()], state()) ->
        {ok, state()} | {error, term(), state()}.
sendmail_2(From, To, Data, State) ->
  sendmail_2(mail_from, #{from => From, to => To, data => Data}, State).

sendmail_2(mail_from, #{from := From} = Mail, State) ->
  Timeout = get_read_timeout_option(State, <<"MAIL FROM">>, 300_000),
  Cmd = smtp_proto:encode_mail_from_cmd(From),
  case exec(State, Cmd, 250, Timeout) of
    {ok, _, NewParser} ->
      sendmail_2(recp_to, Mail, State#{parser => NewParser});
    {error, Reason} ->
      {error, Reason, State}
  end;

sendmail_2(recp_to, #{to := To} = Mail, State) when is_binary(To) ->
  sendmail_2(recp_to, Mail#{to => [To]}, State);
sendmail_2(recp_to, #{to := []} = Mail, State) ->
  sendmail_2(data, Mail, State);
sendmail_2(recp_to, #{to := [To | Rest]} = Mail, State) ->
  Timeout = get_read_timeout_option(State, <<"RCPT TO">>, 300_000),
  Cmd = smtp_proto:encode_rcpt_to_cmd(To),
  case exec(State, Cmd, 250, Timeout) of
    {ok, _, Parser} ->
      sendmail_2(recp_to, Mail#{to := Rest}, State#{parser => Parser});
    {error, Reason} ->
      {error, Reason, State}
  end;

sendmail_2(data, #{data := Data}, State) ->
  Timeout = get_read_timeout_option(State, <<"DATA">>, 120_000),
  case exec(State, <<"DATA\r\n">>, 354, Timeout) of
    {ok, _, Parser} ->
      State2 = State#{parser => Parser},
      EscapedBody =
        re:replace(Data, "^\\.", "..", [global, multiline, {return, iodata}]),
      case exec(State, [EscapedBody, "\r\n.\r\n"], 250, Timeout) of
        {ok, _, Parser2} ->
          {ok, State2#{parser => Parser2}};
        {error, Reason} ->
          {error, Reason, State2}
      end;
    {error, Reason} ->
      {error, Reason, State}
  end.

-spec exec(state(), smtp_proto:command(),
           smtp_reply:code() | [smtp_reply:code()], timeout()) ->
        smtp_parser:parse_result() | {error, term()}.
exec(State, Command, Code, Timeout) when is_integer(Code) ->
  exec(State, Command, [Code], Timeout);
exec(#{transport := T, socket := S, parser := P}, Command, Codes, Timeout) ->
  case send(T, S, Command) of
    ok ->
      case recv(T, S, Timeout, P) of
        {ok, #{code := Code} = Reply, Parser} ->
          case lists:member(Code, Codes) of
            true ->
              {ok, Reply, Parser};
            false ->
              {error, {unexpected_code, Reply, Parser}}
          end;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

-spec send(transport(), Socket, iodata()) ->
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
      {error, {connection_error, Reason}}
  end.

-spec recv(transport(), Socket, timeout(), smtp_parser:parser()) ->
        smtp_parser:parse_result() | {error, term()}
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
          {error, {parse_error, Reason}}
        end;
    {error, Reason} ->
      {error, {connection_error, Reason}}
    end.

-spec get_authentication_option(state()) -> authentication() | error.
get_authentication_option(#{options := Options}) ->
  maps:get(authentication, Options, error).

-spec get_read_timeout_option(state(), binary(), timeout()) -> timeout().
get_read_timeout_option(#{options := Options}, Command, Default) ->
  ReadTimeoutOptions = maps:get(read_timeouts, Options, #{}),
  maps:get(Command, ReadTimeoutOptions, Default).

-spec get_starttls_policy_option(state()) -> starttls_policy().
get_starttls_policy_option(#{options := Options}) ->
  maps:get(starttls, Options, disabled).
