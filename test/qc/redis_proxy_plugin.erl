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
%%% @doc Redis Proxy contract.
%%%

-module(redis_proxy_plugin).
-behaviour(ubf_plugin_stateless).

-include_lib("ubf/include/ubf.hrl").
-include_lib("ubf/include/ubf_plugin_stateless.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_types(ubf_redis_types_plugin).
-add_contract("test/qc/redis_proxy_plugin").

info() ->
    "I am a Redis Proxy server".

description() ->
    "An stateless Redis Proxy server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,start,undefined}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> NewStateData::term()
%% @doc stop handler event
handlerStop(_Pid,_Reason,undefined) ->
    undefine.

%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    case client_rpc(Event) of
        {reply,Reply} ->
            case Event of
                {'HGETALL',_} ->
                    try_to_proplist(Reply);
                'INFO' ->
                    [ list_to_tuple(binary:split(X, <<":">>))
                      || X <- binary:split(Reply, <<"\r\n">>) ];
                {'OBJECT','ENCODING',_} ->
                    try_to_atom(Reply);
                {'TYPE',_} ->
                    case Reply of
                        {ok,X} ->
                            try_to_atom(X);
                        _ ->
                            Reply
                    end;
                {'ZINCRBY',_,_,_} ->
                    try_to_number(Reply);
                _ ->
                    Reply
            end;
        Error ->
            Error
    end.

%%%----------------------------------------------------------------------
%%% Implementation functions
%%%----------------------------------------------------------------------

%% @TODO add safe option
try_to_atom(X) ->
    try to_atom(X)
    catch
        error:badarg ->
            X
    end.

try_to_number(X) ->
    try to_integer(X)
    catch
        error:badarg ->
            try to_float(X)
            catch
                error:badarg ->
                    X
            end
    end.

try_to_proplist(X) ->
    try to_proplist(X)
    catch
        error:_ ->
            X
    end.

to_atom(X) ->
    list_to_atom(binary_to_list(X)).

to_integer(X) ->
    list_to_integer(binary_to_list(X)).

to_float(X) ->
    list_to_float(binary_to_list(X)).

to_proplist([Key,Val|Rest]) ->
    [{Key,Val} | to_proplist(Rest)];
to_proplist([]) ->
     [].

client_start() ->
    Options = [{proto,ruf},{statelessrpc,true},{serverhello,undefined},{simplerpc,true}],
    {ok,Pid,undefined} = ubf_client:connect("127.0.0.1",6379,Options,infinity),
    {ok,Pid}.

client_stop(Pid) ->
    ubf_client:stop(Pid).

client_rpc(Args) ->
    client_rpc(Args,5000).

client_rpc(Args,Timeout) ->
    {ok,Pid} = client_start(),
    try
        ubf_client:rpc(Pid,Args,Timeout)
    after
        client_stop(Pid)
    end.
