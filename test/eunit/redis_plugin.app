%%% -*- mode: erlang -*-
%%%

{application, redis_plugin,
 [
  {description, "REDIS_PLUGIN"},
  {vsn, "0.01"},
  {id, "REDIS_PLUGIN"},
  {modules, [
             %% TODO: fill in this list, perhaps
            ]
  },
  {registered, [ ] },
  %% NOTE: do not list applications which are load-only!
  {applications, [ kernel, stdlib, sasl ] },
  {mod, {redis_plugin_app, []} }
 ]
}.
