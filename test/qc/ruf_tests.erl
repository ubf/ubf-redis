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
%%% Description: Redis Unified Generator Tests
%%%----------------------------------------------------------------------

-module(ruf_tests).

-ifdef(QC).

-include_lib("qc/include/qc.hrl").

-export([qc_run/0, qc_run/1]).
-export([prop_ruf_requests/0]).
-export([prop_ruf_responses/0]).

-define(REDIS_PROXY, redis_proxy_plugin).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

qc_run() ->
    qc_run(3000).

qc_run(NumTests) ->
    qc:module({numtests,NumTests}, ?MODULE).

%% -----------------------------------------------------------------
%% @desc test the requests generator against the ruf encoder/decoder
%% -----------------------------------------------------------------
prop_ruf_requests() ->
    SKIPLIST = [],
    ?FORALL({call,_,_,[_,TypeName,X]},
            ?SUCHTHAT({call,_,_,[_,TN,_]}, (qc_ubf_impl()):command_gen(undefined, input),
                      not lists:member(TN,SKIPLIST)),
            ?LET(Safe, bool(),
                 ?LET(DecodeStops, list(nat()),
                      begin
                          {RUF,A} = encode(client, Safe, X),
                          {Y,B} = decode(server, Safe, RUF, DecodeStops),
                          Res = equal(X,Y,strict),
                          ?WHENFAIL(io:format("~n~p:~p ~p~n~n ~p~n~n ~p~n~n ~p (~w)~n~n ~p~n~n ~p~n~n",
                                              [?FILE, ?LINE, TypeName, X, A, RUF, DecodeStops, Y, B]), Res)
                      end))).

%% -----------------------------------------------------------------
%% @desc test the responses generator against the ruf encoder/decoder
%% -----------------------------------------------------------------
prop_ruf_responses() ->
    SKIPLIST = [],
    ?FORALL({call,_,_,[_,TypeName,X]},
            ?SUCHTHAT({call,_,_,[_,TN,Y]}, (qc_ubf_impl()):command_gen(undefined, output),
                      not lists:member(TN,SKIPLIST) andalso is_legal_response(Y)),
            ?LET(Safe, bool(),
                 ?LET(DecodeStops, list(nat()),
                      begin
                          {RUF,A} = encode(server, Safe, X),
                          {Y,B} = decode(client, Safe, RUF, DecodeStops),
                          Res = equal(X,Y,weak),
                          ?WHENFAIL(io:format("~n~p:~p ~p~n~n ~p~n~n ~p~n~n ~p (~w)~n~n ~p~n~n ~p~n~n",
                                              [?FILE, ?LINE, TypeName, X, A, RUF, DecodeStops, Y, B]), Res)
                      end))).

is_legal_response({ok, undefined}) ->
    true;
is_legal_response({ok, X}) ->
    nomatch == binary:match(X, <<"\r\n">>);
is_legal_response({error, X}) ->
    nomatch == binary:match(X, <<"\r\n">>);
is_legal_response(X) when is_tuple(X) ->
    is_legal_response(tuple_to_list(X));
is_legal_response([H|T]) ->
    is_legal_response(H) andalso is_legal_response(T);
is_legal_response(_X) ->
    true.

%%%-------------------------------------------------------------------
%%% Internal
%%%-------------------------------------------------------------------

qc_ubf_impl() ->
    qc_ubf_impl:new(?MODULE, [?REDIS_PROXY]).

encode(client, Safe, Term) ->
    put('ubf_info', ruf_client_driver),
    encode1(Safe, Term);
encode(server, Safe, Term) ->
    put('ubf_info', ruf_driver),
    encode1(Safe, Term).

decode(client, Safe, Binary, Stops) ->
    put('ubf_info', ruf_client_driver),
    decode1(Safe, Binary, Stops);
decode(server, Safe, Binary, Stops) ->
    put('ubf_info', ruf_driver),
    decode1(Safe, Binary, Stops).

encode1(_Safe, Term) ->
    X = ruf_term:decode(Term, ubf_redis_plugin, "2.4.14.0"),
    {iolist_to_binary(ruf:encode(X, ubf_redis_plugin, "2.4.14.0")), X}.

decode1(Safe, Binary, Stops) ->
    Stops1 = Stops++[byte_size(Binary)],
    decode1(Safe, Binary, ruf:decode_init(Safe, <<>>), Stops1).

decode1(_Safe, Binary, Cont, []) ->
    error(badarg, [Binary, Cont]);
decode1(Safe, Binary, Cont, [H|L]) when H =< byte_size(Binary) ->
    <<Prefix:H/binary,Rest/binary>> = Binary,
    case ruf:decode(Prefix, ubf_redis_plugin, Cont) of
        {ok, X, <<>>, _} ->
            {ruf_term:encode(X, ubf_redis_plugin, "2.4.14.0", Safe), X};
        {more, _}=NewCont ->
            decode1(Safe, Rest, NewCont, L)
    end;
decode1(Safe, Binary, Cont, _) ->
    case ruf:decode(Binary, ubf_redis_plugin, Cont) of
        {ok, X, <<>>, _} ->
            {ruf_term:encode(X, ubf_redis_plugin, "2.4.14.0", Safe), X};
        X ->
            error(badarg, [X])
    end.

equal(X,X,_) ->
    true;
equal(X,Y,weak) when is_atom(X) ->
    try X =:= to_atom(Y)
    catch
        error:badarg ->
            false
    end;
equal(X,Y,weak) when is_integer(X) ->
    try X =:= to_integer(Y)
    catch
        error:badarg ->
            false
    end;
equal(X,Y,weak) when is_float(X) ->
    try X =:= to_float(Y)
    catch
        error:badarg ->
            false
    end;
equal(X,Y,weak) when is_tuple(X) ->
    equal(tuple_to_list(X),Y,weak);
equal(X,Y,weak) when is_list(X), is_list(Y) ->
    X1 = flatten(X),
    if length(X1) == length(Y) ->
            Fun = fun({A,B}) -> equal(A, B, weak) end,
            lists:all(Fun, lists:zip(X1, Y));
       true ->
            false
    end;
equal(_,_,_) ->
    false.

flatten(L) ->
    lists:flatten(flatten1(L)).

flatten1(H) when is_tuple(H) ->
    flatten1(tuple_to_list(H));
flatten1(H) when is_list(H) ->
    [ flatten1(X) || X <- H ];
flatten1(H) ->
    H.

to_atom(X) ->
    list_to_atom(binary_to_list(X)).

to_integer(X) ->
    list_to_integer(binary_to_list(X)).

to_float(X) ->
    list_to_float(binary_to_list(X)).

-endif. %% -ifdef(QC).
