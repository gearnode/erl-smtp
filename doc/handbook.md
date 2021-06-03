# Introduction
This document contains development notes about the `smtp` library.

# Versioning
The following `smtp` versions are available:
- `0.y.z` unstable versions.
- `x.y.z` stable versions: `smtp` will maintain reasonable backward
  compatibility, deprecating features before removing them.
- Experimental untagged versions.

Developers who use unstable or experimental versions are responsible for
updating their application when `smtp` is modified. Note that
unstable versions can be modified without backward compatibility at any
time.

# Client
A client is a single connection to an SMTP server.

## Options
The following client options are available:
- `host`: the hostname or IP address to connect to (default:
  `localhost`).
- `port`: the port number to connect to (default: 25).
- `transport`: the network transport, either `tcp` or `tls`.
- `starttls`: the `starttls` policy (note that this option is ignored if
  `transport` option is set to `tls`); valid values are:
  - `disabled`: do not try to upgrade to secure connection even if the
    server support the extension.
  - `required`: try to upgrade to secure connection and consider failure
    as fatal error.
  - `best_effort`: try to upgrade to secure connection but not consider
    failure as non fatal error.
- `tcp_options`: a list of gen_tcp client options to apply if the
  transport is either `tcp` or `tls`.
- `tls_options`: a list of `ssl` client options to apply if the
  transport is either `tls`.
- `authentication`: credentials to use for authentication (note that
  authentication is disabled when the option is not set). It can be
  provided using one of the following forms:
  - `{Mechanism, #{username => User, password => Password}}` when valid
  mechanism values are:
      - `PLAIN`
      - `LOGIN`
      - `CRAM-MD5`
      - `XOAUTH2`
- `connection_timeout`: the timeout of each read operations when reading
  a response. It can be provided using the following from:
  `#{CommandName => Timeout}` (e.g. `#{<<"HELO">> => 300_000}`).
- `connection_timeout`: the timeout for the initial connection to the
  server.
- `log_requests`: toggle request logging (default: `true`).

For TLS connections, the client uses both the list of TCP options and
the list of TLS options.

## Low-level API
TODO

## High-level API
TODO
