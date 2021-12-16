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

-module(smtp_sasl).

-export([encode_plain/2, encode_login/2, encode_cram_md5/3, encode_xoauth2/2]).

-spec encode_plain(binary(), binary()) -> binary().
encode_plain(Username, Password) when is_binary(Username),
                                      is_binary(Password) ->
  Enc = b64:encode(<<0, Username/binary, 0, Password/binary>>),
  <<Enc/binary, $\r, $\n>>.

-spec encode_login(binary(), binary()) -> {binary(), binary()}.
encode_login(Username, Password) when is_binary(Username),
                                      is_binary(Password) ->
  UsernameEnc = b64:encode(Username),
  PasswordEnc = b64:encode(Password),
  {<<UsernameEnc/binary, $\r, $\n>>, <<PasswordEnc/binary, $\r, $\n>>}.

-spec encode_cram_md5(binary(), binary(), binary()) -> binary().
encode_cram_md5(Username, Password, Challenge) when is_binary(Username),
                                                    is_binary(Password),
                                                    is_binary(Challenge) ->
  Digest = string:lowercase(
             binary:encode_hex(
               crypto:mac(hmac, md5, Password, Challenge))),
  Enc = b64:encode(<<Username/binary, $\s, Digest/binary>>),
  <<Enc/binary, $\r, $\n>>.

-spec encode_xoauth2(binary(), binary()) -> binary().
encode_xoauth2(Username, AccessToken) when is_binary(Username),
                                           is_binary(AccessToken) ->
  Enc = b64:encode(<<"user=", Username/binary, 1,
                     "auth=Bearer", AccessToken/binary, 1, 1>>),
  <<Enc/binary, $\r, $\n>>.
