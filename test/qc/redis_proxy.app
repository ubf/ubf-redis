%%% -*- mode: erlang -*-
%%%

{application, redis_proxy,
 [
  {description, "REDIS_PROXY"},
  {vsn, "0.01"},
  {id, "REDIS_PROXY"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl] },
  {mod, {redis_proxy_app, []} }
 ]
}.
