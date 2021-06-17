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

-export([sendmail/3]).

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

-spec sendmail(binary(), imf:message(), map()) ->
        ok | {error, term()}.
sendmail(Sender, Mail, Options) ->
  Data = imf:encode(Mail),
  Recipients = imf:recipient_addresses(Mail),
    case smtp_client:start_link(Options) of
      {ok, Ref} ->
        try
          ok = mail_from(Ref, Sender),
          ok = recp_to(Ref, Recipients),
          ok = data(Ref, Data)
        catch
          throw:{error, Reason} ->
            smtp_client:quit(Ref),
            {error, Reason};
          exit:{noproc, _Trace} ->
            {error, connection_failure};
          exit:{ExitReason, _Trace} ->
            {error, ExitReason}
        end,
        smtp_client:quit(Ref);
      {error, Reason} ->
        {error, Reason}
    end.

mail_from(Ref, Sender) ->
  case smtp_client:mail_from(Ref, Sender) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, Reason})
  end.

recp_to(Ref, Recipients) ->
  case smtp_client:recp_to(Ref, Recipients) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, Reason})
  end.

data(Ref, Data) ->
  case smtp_client:data(Ref, Data) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, Reason})
  end.
