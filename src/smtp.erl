%% Copyright (c) 2022 Bryan Frimin <bryan@frimin.fr>.
%% Copyright (c) 2021-2022 Exograd SAS.
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

-module(smtp).

-include_lib("kernel/include/inet.hrl").

-export([default_port/0, default_tls_port/0, default_port/1,
         fqdn/0]).

-export([sendmail/2, sendmail/3,
         sendmail_raw/3, sendmail_raw/4]).

-export_type([protocol/0, pool_id/0, transport/0]).

-type protocol() :: smtp | submission | smtps.

-type pool_id() :: atom().

-type transport() :: tcp | tls.

-type sendmail_options() :: #{pool => pool_id()}.

-spec default_port() -> inet:port_number().
default_port() ->
  default_port(smtp).

-spec default_tls_port() -> inet:port_number().
default_tls_port() ->
  default_port(smtps).

-spec default_port(protocol()) -> inet:port_number().
default_port(smtp) ->
  25;
default_port(submission) ->
  587;
default_port(smtps) ->
  465.

-spec fqdn() -> uri:host().
fqdn() ->
  {ok, Hostname} = inet:gethostname(),
  case inet:gethostbyname(Hostname) of
    {ok, #hostent{h_name = FQDN}} when is_atom(FQDN) ->
      atom_to_binary(FQDN);
    {ok, #hostent{h_name = FQDN}} when is_list(FQDN) ->
      iolist_to_binary(FQDN);
    {error, _} ->
      <<"localhost">>
  end.

-spec sendmail(binary(), imf:message()) -> ok | {error, term()}.
sendmail(Sender, Mail) ->
  sendmail(Sender, Mail, #{}).

-spec sendmail(binary(), imf:message(), sendmail_options()) ->
        ok | {error, term()}.
sendmail(Sender, Mail, Options) ->
  Recipients = imf:recipient_addresses(Mail),
  Data = imf:encode(Mail),
  sendmail_raw(Sender, Recipients, Data, Options).

-spec sendmail_raw(binary(), [binary()], iodata()) ->
        ok | {error, term()}.
sendmail_raw(Sender, Recipients, Data) ->
  sendmail_raw(Sender, Recipients, Data, #{}).

-spec sendmail_raw(binary(), [binary()], iodata(), sendmail_options()) ->
        ok | {error, term()}.
sendmail_raw(Sender, Recipients, Data, Options) ->
  PoolId = maps:get(pool, Options, default),
  PoolRef = smtp_pool:process_name(PoolId),
  smtp_pool:sendmail(PoolRef, Sender, Recipients, Data).
