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
%%% @doc Protocol driver process for RUF (Redis Binary Format)
%%% protocol sessions.

-module(ruf_driver).
-behaviour(contract_driver).

-export([start/1, start/2, init/1, init/2, encode/3, decode/5]).

%%%=========================================================================
%%%  Records, Types, Macros
%%%=========================================================================

-define(VSN_1, "2.4.15.0").

%%%=========================================================================
%%%  API
%%%=========================================================================

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
start(Contract) ->
    start(Contract, []).

start(Contract, Options) ->
    proc_utils:spawn_link_debug(fun() -> contract_driver:start(?MODULE, Contract, Options) end, ruf_client_driver).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
init(Contract) ->
    init(Contract, []).

init(_Contract, Options) ->
    Safe = safe(Options),
    {Safe, ruf:decode_init(Safe)}.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode(Contract, _Safe, Term) ->
    VSN = case get(?MODULE) of undefined -> ?VSN_1; V -> V end,
    ruf:encode(ruf_term:decode(Term, Contract, VSN), Contract, VSN).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode(Contract, Safe, Cont, Binary, CallBack) ->
    Cont1 = ruf:decode(Binary, Contract, Cont),
    decode(Contract, Safe, Cont1, CallBack).

decode(Contract, Safe, {ok, Term, Binary, VSN}=_Cont, CallBack) ->
    put(?MODULE, VSN),
    CallBack(ruf_term:encode(Term, Contract, VSN, Safe)),
    Cont1 = ruf:decode_init(Safe, Binary),
    decode(Contract, Safe, Cont1, CallBack);
decode(_Contract, _Safe, Cont, _CallBack) ->
    Cont.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
safe(Options) ->
    proplists:get_bool(safe, Options).
