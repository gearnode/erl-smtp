%% Copyright (c) 2021 Exograd SAS.
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

-module(smtp_pool).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([process_name/1, start_link/2, stop/1]).

-export([sendmail/3]).

-export_type([ref/0, options/0]).

-type ref() :: c_gen_server:ref().

-type state() :: #{id := smtp:pool_id(),
                   options := options()}.

-type options() :: #{client_options => smtp_client:options()}.

-spec process_name(smtp:pool_id()) -> atom().
process_name(Id) ->
  Name = <<"smtp_pool_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(smtp:pool_id(), options()) -> c_gen_server:start_ret().
start_link(Id, Options) ->
  Name = process_name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Id, Options], []).

-spec stop(smtp:pool_id()) -> ok.
stop(Id) ->
  Name = process_name(Id),
  gen_server:stop(Name).

-spec sendmail(ref(), binary(), imf:message()) -> ok | {error, term()}.
sendmail(Ref, Sender, Mail) ->
  Data = imf:encode(Mail),
  Recipients = imf:recipient_addresses(Mail),
  gen_server:call(Ref, {sendmail, Sender, Recipients, Data}, infinity).

-spec init(list()) -> c_gen_server:init_ret(state()).
init([Id, Options]) ->
  logger:update_process_metadata(#{domain => [smtp, pool, Id]}),
  process_flag(trap_exit, true),
  State = #{id => Id, options => #{client_options => Options}},
  {ok, State}.

-spec handle_call(term(), {pid(), c_gen_server:request_id()}, state()) ->
        c_gen_server:handle_call_ret(state()).
handle_call({sendmail, Sender, Recipients, Data}, _,
            #{options := Options} = State) ->
  ClientOptions = maps:get(client_options, Options, #{}),
  case smtp_client:start_link(ClientOptions) of
    {ok, Client} ->
      try
        case smtp_client:mail_from(Client, Sender) of
          ok ->
            case smtp_client:recp_to(Client, Recipients) of
              ok ->
                case smtp_client:data(Client, Data) of
                  ok ->
                    smtp_client:quit(Client),
                    smtp_client:stop(Client),
                    {reply, ok, State};
                  {error, _} = Error ->
                    smtp_client:quit(Client),
                    smtp_client:stop(Client),
                    {reply, Error, State}
                end;
              {error,  _} = Error ->
                smtp_client:quit(Client),
                smtp_client:stop(Client),
                {reply, Error, State}
            end;
          {error, _} = Error ->
            smtp_client:quit(Client),
            smtp_client:stop(Client),
            {reply, Error, State}
        end
      catch
        exit:{noproc, _Trace} ->
          {reply, {error, connection_failure}, State};
        exit:{ExitReason, _Trace} ->
          {reply, {error, ExitReason}, State}
      end;
    {error, Reason} ->
      {reply, {error, {client_error, Reason}}, State}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> c_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> c_gen_server:handle_info_ret(state()).
handle_info({'EXIT', _, _}, State) ->
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.
