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

-module(smtp).

-include_lib("kernel/include/inet.hrl").

-export([default_port/0, default_tls_port/0, default_port/1,
         fqdn/0]).

-export_type([protocol/0]).

-type protocol() :: smtp | submission | smtps.

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

-spec fqdn() -> binary().
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
