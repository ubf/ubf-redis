%%% -*- mode: erlang -*-
%%%
%%% The MIT License
%%%
%%% Copyright (C) 2012 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%

%%%----------------------------------------------------------------------
%%% File    : redis_plugin_sup.erl
%%% Purpose : test UBF top-level supervisor
%%%----------------------------------------------------------------------

-module(redis_plugin_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main TEST UBF supervisor.

init(Args) ->
    %% seq_trace:set_token(send, true), seq_trace:set_token('receive', true),

    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    DefaultMaxConn = 10000,
    DefaultTimeout = 60000,
    DefaultPlugins = proplists:get_value(plugins, Args, [ubf_redis_plugin]),

    CRUF = case proplists:get_value(test_ruf_tcp_port, Args, 0) of
               undefined ->
                   [];
               RUFPort ->
                   RUFMaxConn = proplists:get_value(test_ruf_maxconn, Args, DefaultMaxConn),
                   RUFIdleTimer = proplists:get_value(test_ruf_timeout, Args, DefaultTimeout),
                   RUFOptions = [{statelessrpc,true}               %% mandatory for redis
                                 , {startplugin,ubf_redis_plugin}  %%          "
                                 , {serverhello,undefined}         %%          "
                                 , {simplerpc,true}                %%          "
                                 , {proto,ruf}                     %%          "
                                 , {maxconn,RUFMaxConn}
                                 , {idletimer,RUFIdleTimer}
                                 , {registeredname,test_ruf_tcp_port}
                                ],
                   RUFServer =
                       {ruf_server, {ubf_server, start_link, [test_ruf, DefaultPlugins, RUFPort, RUFOptions]},
                        permanent, 2000, worker, [ruf_server]},

                   [RUFServer]
           end,

    {ok, {{one_for_one, 2, 60}, CRUF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
