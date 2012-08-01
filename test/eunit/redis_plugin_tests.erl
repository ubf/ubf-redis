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

-module(redis_plugin_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf.hrl").

-define(APPLICATION, redis_plugin).
-define(RUF_PORT, server_port(test_ruf_tcp_port)).

-define(SLEEP, 50).

-record(args, {host, port, proto}).


%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun () -> test_setup(?APPLICATION) end,
               fun (X) -> test_teardown(X) end
              ).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (all_actual_tests_(ruf))(not_used)
    }.

all_actual_tests_(ruf=Proto) ->
    all_actual_tests_("localhost",fun() -> ?RUF_PORT end,Proto).

all_actual_tests_(Host,Port,Proto) ->
    fun(_) ->
            [?_test(test_001(#args{host=Host,port=Port(),proto=Proto}))
             , ?_test(test_002(#args{host=Host,port=Port(),proto=Proto}))
             , ?_test(test_003(#args{host=Host,port=Port(),proto=Proto}))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup(App) ->
    application:start(sasl),
    application:stop(App),
    true = code:add_patha("../test/eunit"),
    ok = application:start(App),
    App.

test_teardown(App) ->
    application:stop(App),
    true = code:del_path("../test/eunit"),
    ok.

%% connect -> close
test_001(#args{host=Host,port=Port}) ->
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    ok = gen_tcp:close(Sock).

%% connect -> shutdown(X) -> close
test_002(Args) ->
    test_002(Args,read),
    test_002(Args,write),
    test_002(Args,read_write).

test_002(#args{host=Host,port=Port},How) ->
    {ok,Sock} = gen_tcp:connect(Host,Port,[]),
    ok = gen_tcp:shutdown(Sock, How),
    ok = gen_tcp:close(Sock).

%% UBF 'synchronous' keepalive
test_003(#args{}=Args) ->
    {ok,Pid1} = client_connect(Args),
    {reply,{ok,<<"PONG">>}} = client_rpc(Pid1, 'PING'),
    client_stop(Pid1).

%%%----------------------------------------------------------------------
%%% Helpers
%%%----------------------------------------------------------------------

server_port(Name) ->
    case proc_socket_server:server_port(Name) of
        Port when is_integer(Port) ->
            Port;
        _ ->
            timer:sleep(10),
            server_port(Name)
    end.

client_connect(#args{host=Host,port=Port,proto=Proto}) ->
    Options = [{proto,Proto},{statelessrpc,true},{serverhello,undefined},{simplerpc,true}],
    {ok,Pid,undefined} = ubf_client:connect(Host,Port,Options,infinity),
    {ok,Pid}.

client_rpc(X,Y) ->
    client_rpc(X,Y,infinity).

client_rpc(Pid,Args,Timeout) ->
    ubf_client:rpc(Pid,Args,Timeout).

client_event(Pid,Msg) ->
    ubf_client:sendEvent(Pid, Msg).

client_install_single_callback(Pid) ->
    Caller = self(),
    Ref = {Pid, make_ref()},
    Fun = fun(Msg) -> Caller ! {Ref, Msg}, ubf_client:install_default_handler(Pid) end,
    ack = ubf_client:install_handler(Pid, Fun),
    Ref.

client_expect_callback(_Pid, Ref) ->
    receive {Ref, Msg} -> Msg end.

client_stop(Pid) ->
    ubf_client:stop(Pid).
