%%% -*- mode: erlang -*-
%%%
%%% The MIT License
%%%
%%% Copyright (C) 2012-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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
%%% @doc Sample Redis contract.
%%%

-module(ubf_redis_plugin).
-behaviour(ubf_plugin_stateless).

%% Required callback API for all UBF contract implementations.
-export([info/0, description/0, keepalive/0]).
-export([moduleStart/1, moduleRestart/1]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1, handlerEvent/1]).

-import(ubf_plugin_handler, [sendEvent/2, install_handler/2]).

-compile({parse_transform,contract_parser}).
-add_types(ubf_redis_types_plugin).
-add_contract("src/ubf_redis_plugin").

-include_lib("ubf/include/ubf.hrl").

info() ->
    "I am a Redis server".

description() ->
    "A Redis server programmed by UBF".

keepalive() ->
    ok.

%% @doc start module
moduleStart(_Args) ->
    unused.

%% @doc restart module
moduleRestart(Args) ->
    moduleStart(Args).

%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    ack = install_handler(self(), fun handlerEvent/1),
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> none()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc('PING'=_Arg) ->
    {ok, <<"PONG">>};
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.

handlerEvent(Event) ->
    %% @TODO add your own implementation here
    %% Let's fake it and echo the request
    sendEvent(self(), Event),
    fun handlerEvent/1.
