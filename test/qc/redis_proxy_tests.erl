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
%%% Description: Redis Proxy Tests
%%%----------------------------------------------------------------------

-module(redis_proxy_tests).

-ifdef(QC).

-export([qc_run/0, qc_run/1, qc_run/2]).
-export([qc_counterexample/0, qc_counterexample/1, qc_counterexample/2]).

-export([setup/0, setup/1]).
-export([command_typename/3]).
-export([aggregate/1]).

-include_lib("ubf/include/qc_ubf.hrl").

-define(REDIS_PROXY, redis_proxy_plugin).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

qc_run() ->
    qc_run(500).

qc_run(NumTests) ->
    qc_run(NumTests, []).

qc_run(NumTests, Options) ->
    %% close port (previous tests)
    ok = setup(),
    %% check for running redis-server
    case gen_tcp:listen(6379, []) of
        {ok,Port} ->
            ok = gen_tcp:close(Port),
            test_setup(),
            try
                qc_ubf:qc_run(?MODULE, [?REDIS_PROXY], NumTests, Options)
            after
                test_teardown()
            end;
        {error,eaddrinuse} ->
            _ = os:cmd("killall -9 redis-server"),
            qc_run(NumTests, Options)
    end.

qc_counterexample() ->
    qc_counterexample([]).

qc_counterexample(Options) ->
    qc_counterexample(Options, ?QC:counterexample()).

qc_counterexample(Options, CounterExample) ->
    ?QC:check(qc_ubf:qc_prop(?MODULE, [?REDIS_PROXY], Options), CounterExample).

setup() ->
    {ok, _} = redis_server_teardown(),
    {ok, _} = redis_server_setup(),
    ok.

setup(_) ->
    redis_server_reset().

command_typename(_S, _Contract, TypeNames) ->
    SkipList =
        [blpop_req,brpop_req,brpoplpush_req]
        ++ [zinterstore_req,zunionstore_req]
        ++ [psubscribe_req,publish_req,punsubscribe_req,subscribe_req,unsubscribe_req]
        ++ [exec_req,unwatch_req]
        ++ [debug_segfault_req,flushall_req,quit_req,bgsave_req,save_req,shutdown_req,sync_req],
    oneof(TypeNames -- SkipList).

aggregate(L) ->
    [ begin
          TypeName = filter_args(Cmd),
          {TypeName, '->', filter_reply(TypeName,Reply)}
      end || {_N,Cmd,Reply,_State} <- L ].

%%%-------------------------------------------------------------------
%%% Internal
%%%-------------------------------------------------------------------

test_setup() ->
    test_setup(redis_proxy).

test_setup(App) ->
    _ = application:start(sasl),
    _ = application:stop(App),
    true = code:add_patha("../test/qc"),
    ok = application:start(App),
    App.

test_teardown() ->
    test_teardown(redis_proxy).

test_teardown(App) ->
    _ = application:stop(App),
    true = code:del_path("../test/qc"),
    ok.

redis_server_teardown() ->
    %% close redis_server
    case whereis(redis_server) of
        undefined ->
            noop;
        Port when is_port(Port) ->
            port_close(Port)
    end,
    {ok,undefined}.

redis_server_setup() ->
    Self = self(),
    {Pid,Ref} = spawn_monitor(fun() -> redis_server_init(Self) end),
    receive
        {Self, Pid, NewPort} ->
            demonitor(Ref, [flush]),
            {ok,NewPort};
        {'DOWN', Ref, _, _, _}=Msg ->
            exit(Msg)
    end.

redis_server_reset() ->
    {ok, _} = rpc('FLUSHALL'),
    {ok, undefined}.

redis_server_init(Parent) ->
    %% remove data
    _ = os:cmd("rm -f /usr/local/var/db/redis/*"),
    %% open port
    Options = [{args, ["/usr/local/etc/redis.conf"]}, binary, stderr_to_stdout, {line,1024}],
    Port = open_port({spawn_executable, "/usr/local/bin/redis-server"}, Options),
    register(redis_server, Port),
    Parent ! {Parent, self(), Port},
    redis_server_loop(Port).

redis_server_loop(Port) ->
    receive
        {Port, eof} ->
            ok;
        {Port, closed} ->
            ok;
        {Port, X} ->
            io:format("~p: ~p~n", [Port, X]),
            redis_server_loop(Port)
    end.

filter_args({set,{var,_},{call,_Mod,_Cmd,[_,TypeName,_]}}) ->
    TypeName.

filter_reply(config_set_req,{error,_Reply}) ->
    'error()';
filter_reply(_Cmd, Reply) ->
    filter_reply(Reply).

filter_reply({ok,_}) ->
    ok;
filter_reply({error,Msg}) ->
    {error,list_to_atom(binary_to_list(Msg))};
filter_reply(X) when is_atom(X) ->
    X;
filter_reply(X) when is_integer(X) ->
    'integer()';
filter_reply(X) when is_float(X) ->
    'float()';
filter_reply(X) when is_binary(X) ->
    'binary()';
filter_reply(X) when is_list(X) ->
    'list()'.

rpc(Call) ->
    case ubf_client:lpc(?REDIS_PROXY, Call) of
        {reply,Reply,_State} ->
            Reply;
        Err ->
            erlang:error(Err)
    end.

-endif. %% -ifdef(QC).
