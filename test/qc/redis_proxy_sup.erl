%%% -*- mode: erlang -*-
%%%
%%% The MIT License
%%%
%%% Copyright (C) 2012-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% File    : redis_proxy_sup.erl
%%% Purpose : test redis proxy top-level supervisor
%%%----------------------------------------------------------------------

-module(redis_proxy_sup).

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
    DefaultPlugins = proplists:get_value(plugins, Args, [redis_proxy_plugin]),

    CRPXY = case proplists:get_value(redis_proxy_tcp_port, Args, 0) of
               undefined ->
                   [];
               RPXYPort ->
                   RPXYMaxConn = proplists:get_value(redis_proxy_maxconn, Args, DefaultMaxConn),
                   RPXYIdleTimer = proplists:get_value(redis_proxy_timeout, Args, DefaultTimeout),
                   RPXYOptions = [{statelessrpc,true}              %% mandatory for redis
                                 , {startplugin,redis_proxy_plugin}%%          "
                                 , {serverhello,undefined}         %%          "
                                 , {simplerpc,true}                %%          "
                                 , {proto,ruf}                     %%          "
                                 , {maxconn,RPXYMaxConn}
                                 , {idletimer,RPXYIdleTimer}
                                 , {registeredname,redis_proxy_tcp_port}
                                ],
                   RPXYServer =
                       {redis_proxy_server, {ubf_server, start_link, [redis_proxy, DefaultPlugins, RPXYPort, RPXYOptions]},
                        permanent, 2000, worker, [redis_proxy_server]},

                   [RPXYServer]
           end,

    {ok, {{one_for_one, 2, 60}, CRPXY}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
