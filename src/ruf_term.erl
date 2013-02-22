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

-module(ruf_term).

%%% ---------------------------------------------------
%%% @doc Redis Unified Format - Erlang Term Format
%%%
%%% This module implements a decoder and encoder between Redis
%%% Unified Format and Erlang Term Format.
%%%
%%% NOTE: This module acts as bridge between Redis and Erlang
%%% conventions for requests and responses.
%%%
%%% @end
%%% ---------------------------------------------------

-include_lib("ubf/include/ubf.hrl").

%% API
-export([encode/4]).
-export([decode/3]).

%%%=========================================================================
%%%  Records, Types, Macros
%%%=========================================================================

-type ruf() :: ruf:ruf().

%%%=========================================================================
%%%  API
%%%=========================================================================

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
-spec encode(Input::ruf(), module(), VSN::undefined | string(), Safe::boolean()) -> term() | no_return().
encode({ok,_}=X, _Contract, _VSN, _Safe) ->
    X;
encode({error,_}=X, _Contract, _VSN, _Safe) ->
    X;
encode(X, _Contract, _VSN, _Safe) when is_integer(X) ->
    X;
encode(X, _Contract, _VSN, Safe) ->
    case get('ubf_info') of
        ruf_client_driver ->
            X;
        ruf_driver ->
            [H|T] = X,
            encode(to_atom(H,Safe), T, Safe);
        I ->
            error(badarg, [I])
    end.



%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
-spec decode(Input::term(), module(), VSN::undefined | string()) -> ruf() | no_return().
decode({ok,_}=X, _Contract, _VSN) ->
    X;
decode({error,_}=X, _Contract, _VSN) ->
    X;
decode(X, _Contract, _VSN) when is_integer(X) ->
    X;
decode(X, _Contract, _VSN) ->
    decode(X).

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------

%% keys
encode('DEL'=X,As, _Safe) ->
    {X,As};
encode('DUMP'=X, [A1], _Safe) ->
    {X,A1};
encode('EXISTS'=X, [A1], _Safe) ->
    {X,A1};
encode('EXPIRE'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('EXPIREAT'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('KEYS'=X, [A1], _Safe) ->
    {X,A1};
encode('MIGRATE'=X, [A1,A2,A3,A4,A5], _Safe) ->
    {X,A1,to_integer(A2),A3,to_integer(A4),to_integer(A5)};
encode('MOVE'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('OBJECT', [<<"REFCOUNT">>,A1], _Safe) ->
    {'OBJECT','REFCOUNT',A1};
encode('OBJECT', [<<"ENCODING">>,A1], _Safe) ->
    {'OBJECT','ENCODING',A1};
encode('OBJECT', [<<"IDLETIME">>,A1], _Safe) ->
    {'OBJECT','IDLETIME',A1};
encode('PERSIST'=X, [A1], _Safe) ->
    {X,A1};
encode('PEXPIRE'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('PEXPIREAT'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('PTTL'=X, [A1], _Safe) ->
    {X,A1};
encode('RANDOMKEY'=X, [], _Safe) ->
    X;
encode('RENAME'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('RENAMENX'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('RESTORE'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('SORT'=X,[A1|A2], Safe) ->
    {X,A1,parse_sort_options(A2,Safe)};
encode('TTL'=X, [A1], _Safe) ->
    {X,A1};
encode('TYPE'=X, [A1], _Safe) ->
    {X,A1};
%% strings
encode('APPEND'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('BITCOUNT'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),to_integer(A3)};
encode('BITOP', [<<"AND">>,A1|A2], _Safe) ->
    {'BITOP','AND',A1,A2};
encode('BITOP', [<<"OR">>,A1|A2], _Safe) ->
    {'BITOP','OR',A1,A2};
encode('BITOP', [<<"XOR">>,A1|A2], _Safe) ->
    {'BITOP','XOR',A1,A2};
encode('BITOP', [<<"NOT">>,A1,A2], _Safe) ->
    {'BITOP','NOT',A1,A2};
encode('DECR'=X, [A1], _Safe) ->
    {X,A1};
encode('DECRBY'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('GET'=X, [A1], _Safe) ->
    {X,A1};
encode('GETBIT'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('GETRANGE'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),to_integer(A3)};
encode('GETSET'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('INCR'=X, [A1], _Safe) ->
    {X,A1};
encode('INCRBY'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('INCRBYFLOAT'=X, [A1,A2], _Safe) ->
    {X,A1,to_number(A2)};
encode('MGET'=X,As, _Safe) ->
    {X,As};
encode('MSET'=X,As, _Safe) ->
    {X,list_to_proplist(As)};
encode('MSETNX'=X,As, _Safe) ->
    {X,list_to_proplist(As)};
encode('PSETEX'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('SET'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('SETBIT'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('SETEX'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('SETNX'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('SETRANGE'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('STRLEN'=X, [A1], _Safe) ->
    {X,A1};
%% hashes
encode('HDEL'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('HEXISTS'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('HGET'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('HGETALL'=X, [A1], _Safe) ->
    {X,A1};
encode('HINCRBY'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,to_integer(A3)};
encode('HINCRBYFLOAT'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,to_float(A3)};
encode('HKEYS'=X, [A1], _Safe) ->
    {X,A1};
encode('HLEN'=X, [A1], _Safe) ->
    {X,A1};
encode('HMGET'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('HMSET'=X, [A1|A2], _Safe) ->
    {X,A1,list_to_proplist(A2)};
encode('HSET'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,A3};
encode('HSETNX'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,A3};
encode('HVALS'=X, [A1], _Safe) ->
    {X,A1};
%% lists
encode('BLPOP'=X, As, _Safe) ->
    {A1,A2} = splitlast(As),
    {X,A1,to_integer(A2)};
encode('BRPOP'=X, As, _Safe) ->
    {A1,A2} = splitlast(As),
    {X,A1,to_integer(A2)};
encode('BRPOPLPUSH'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,to_integer(A3)};
encode('LINDEX'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('LINSERT'=X, [A1,A2,A3,A4], Safe) ->
    {X,A1,to_atom(A2,Safe),A3,A4};
encode('LLEN'=X, [A1], _Safe) ->
    {X,A1};
encode('LPOP'=X, [A1], _Safe) ->
    {X,A1};
encode('LPUSH'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('LPUSHX'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('LRANGE'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),to_integer(A3)};
encode('LREM'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('LSET'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),A3};
encode('LTRIM'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),to_integer(A3)};
encode('RPOP'=X, [A1], _Safe) ->
    {X,A1};
encode('RPOPLPUSH'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('RPUSH'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('RPUSHX'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
%% sets
encode('SADD'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('SCARD'=X, [A1], _Safe) ->
    {X,A1};
encode('SDIFF'=X, A1, _Safe) ->
    {X,A1};
encode('SDIFFSTORE'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('SINTER'=X, A1, _Safe) ->
    {X,A1};
encode('SINTERSTORE'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('SISMEMBER'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('SMEMBERS'=X, [A1], _Safe) ->
    {X,A1};
encode('SMOVE'=X, [A1,A2,A3], _Safe) ->
    {X,A1,A2,A3};
encode('SPOP'=X, [A1], _Safe) ->
    {X,A1};
encode('SRANDMEMBER'=X, [A1], _Safe) ->
    {X,A1};
encode('SREM'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('SUNION'=X, A1, _Safe) ->
    {X,A1};
encode('SUNIONSTORE'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
%% sorted sets
encode('ZADD'=X, [A1|A2], _Safe) ->
    {X,A1,list_to_proplist(A2, fun to_number/1, fun identity/1)};
encode('ZCARD'=X, [A1], _Safe) ->
    {X,A1};
encode('ZCOUNT'=X, [A1,A2,A3], Safe) ->
    {X,A1,to_scoremin(A2,Safe),to_scoremax(A3,Safe)};
encode('ZINCRBY'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_number(A2),A3};
encode('ZINTERSTORE'=X, [A1,A2|As], Safe) ->
    {A3,A4} = parse_zstore_options(As,Safe),
    {X,A1,to_integer(A2),A3,A4};
encode('ZRANGE'=X, [A1,A2,A3|As], Safe) ->
    A4 = parse_zrange_options(As,Safe),
    {X,A1,to_integer(A2),to_integer(A3),A4};
encode('ZRANGEBYSCORE'=X, [A1,A2,A3|As], Safe) ->
    A4 = parse_zrangebyscore_options(As,Safe),
    {X,A1,to_scoremin(A2,Safe),to_scoremax(A3,Safe),A4};
encode('ZRANK'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('ZREM'=X, [A1|A2], _Safe) ->
    {X,A1,A2};
encode('ZREMRANGEBYRANK'=X, [A1,A2,A3], _Safe) ->
    {X,A1,to_integer(A2),to_integer(A3)};
encode('ZREMRANGEBYSCORE'=X, [A1,A2,A3], Safe) ->
    {X,A1,to_scoremin(A2,Safe),to_scoremax(A3,Safe)};
encode('ZREVRANGE'=X, [A1,A2,A3|As], Safe) ->
    A4 = parse_zrange_options(As,Safe),
    {X,A1,to_integer(A2),to_integer(A3),A4};
encode('ZREVRANGEBYSCORE'=X, [A1,A2,A3|As], Safe) ->
    A4 = parse_zrangebyscore_options(As,Safe),
    {X,A1,to_scoremin(A2,Safe),to_scoremax(A3,Safe),A4};
encode('ZREVRANK'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('ZSCORE'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('ZUNIONSTORE'=X, [A1,A2|As], Safe) ->
    {A3,A4} = parse_zstore_options(As,Safe),
    {X,A1,to_integer(A2),A3,A4};
%% publish/subscribe
encode('PSUBSCRIBE'=X, A1, _Safe) ->
    {X,A1};
encode('PUBLISH'=X, [A1,A2], _Safe) ->
    {X,A1,A2};
encode('PUNSUBSCRIBE'=X, A1, _Safe) ->
    {X,A1};
encode('SUBSCRIBE'=X, A1, _Safe) ->
    {X,A1};
encode('UNSUBSCRIBE'=X, A1, _Safe) ->
    {X,A1};
%% transactions
encode('DISCARD'=X, [], _Safe) ->
    X;
encode('EXEC'=X, [], _Safe) ->
    X;
encode('MULTI'=X, [], _Safe) ->
    X;
encode('UNWATCH'=X, [], _Safe) ->
    X;
encode('WATCH'=X, A1, _Safe) ->
    {X,A1};
%% scripting
%% @TODO
%% connection
encode('AUTH'=X, [A1], _Safe) ->
    {X,A1};
encode('ECHO'=X, [A1], _Safe) ->
    {X,A1};
encode('PING'=X, [], _Safe) ->
    X;
encode('QUIT'=X, [], _Safe) ->
    X;
encode('SELECT'=X, [A1], _Safe) ->
    {X,to_integer(A1)};
%% server
encode('BGREWRITEAOF'=X, [], _Safe) ->
    X;
encode('BGSAVE'=X, [], _Safe) ->
    X;
encode('CONFIG', [<<"GET">>,A1], _Safe) ->
    {'CONFIG','GET',A1};
encode('CONFIG', [<<"RESETSTAT">>], _Safe) ->
    {'CONFIG','RESETSTAT'};
encode('CONFIG', [<<"SET">>,A1,A2], _Safe) ->
    {'CONFIG','SET',A1,A2};
encode('DBSIZE'=X, [], _Safe) ->
    X;
encode('DEBUG', [<<"OBJECT">>,A1], _Safe) ->
    {'DEBUG','OBJECT',A1};
encode('DEBUG', [<<"SEGFAULT">>], _Safe) ->
    {'DEBUG','SEGFAULT'};
encode('FLUSHALL'=X, [], _Safe) ->
    X;
encode('FLUSHDB'=X, [], _Safe) ->
    X;
encode('INFO'=X, [], _Safe) ->
    X;
encode('LASTSAVE'=X, [], _Safe) ->
    X;
encode('MONITOR'=X, [], _Safe) ->
    X;
encode('SAVE'=X, [], _Safe) ->
    X;
encode('SHUTDOWN'=X, [A1], Safe) ->
    {X,to_atom(A1,Safe)};
encode('SLAVEOF'=X, [<<"no">>,<<"one">>], _Safe) ->
    {X,no,one};
encode('SLAVEOF'=X, [A1,A2], _Safe) ->
    {X,A1,to_integer(A2)};
encode('SYNC'=X, [], _Safe) ->
    X;
encode('TIME'=X, [], _Safe) ->
    X.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode(undefined) ->
    undefined;
decode(X) when is_binary(X) ->
    X;
decode(X) when is_integer(X) ->
    from_integer(X);
decode(X) when is_float(X) ->
    from_float(X);
decode(X) when is_atom(X) ->
    from_atom(X);
decode({X,Y}) when X==ok; X==error ->
    {X,decode(Y)};
decode({'(',X}) when is_number(X) ->
    Y = decode(X),
    <<<<$(>>/binary,Y/binary>>;
decode(X) when is_tuple(X) ->
    [ decode(Y) || Y <- tuple_to_list(X) ];
decode([]) ->
    [];
decode([{X,Y}|T]) when X==ok; X==error ->
    [{X,decode(Y)}|decode(T)];
decode([{X,Y}|T]) ->
    [decode(X),decode(Y)|decode(T)];
decode([H|T]) ->
    [decode(H)|decode(T)].

%%%========================================================================
%%% Utility functions
%%%========================================================================

parse_sort_options(L,Safe) ->
    parse_sort_options(L,Safe,[]).

parse_sort_options([],_Safe,Acc) ->
    lists:reverse(Acc);
parse_sort_options([<<"BY">>,X|L],Safe,Acc) ->
    parse_sort_options(L,Safe,[{'BY',X}|Acc]);
parse_sort_options([<<"LIMIT">>,X,Y|L],Safe,Acc) ->
    parse_sort_options(L,Safe,[{'LIMIT',to_integer(X),to_integer(Y)}|Acc]);
parse_sort_options([<<"GET">>,X|L],Safe,Acc) ->
    parse_sort_options(L,Safe,[{'GET',X}|Acc]);
parse_sort_options([<<"ASC">>|L],Safe,Acc) ->
    parse_sort_options(L,Safe,['ASC'|Acc]);
parse_sort_options([<<"DESC">>|L],Safe,Acc) ->
    parse_sort_options(L,Safe,['DESC'|Acc]);
parse_sort_options([<<"ALPHA">>|L],Safe,Acc) ->
    parse_sort_options(L,Safe,['ALPHA'|Acc]);
parse_sort_options([<<"STORE">>,X|L],Safe,Acc) ->
    parse_sort_options(L,Safe,[{'STORE',X}|Acc]).

parse_zrange_options(L,Safe) ->
    parse_zrange_options(L,Safe,[]).

parse_zrange_options([],_Safe,Acc) ->
    lists:reverse(Acc);
parse_zrange_options([<<"WITHSCORES">>|T],Safe,Acc) ->
    parse_zrange_options(T,Safe,['WITHSCORES'|Acc]).

parse_zrangebyscore_options(L,Safe) ->
    parse_zrangebyscore_options(L,Safe,[]).

parse_zrangebyscore_options([],_Safe,Acc) ->
    lists:reverse(Acc);
parse_zrangebyscore_options([<<"WITHSCORES">>|T],Safe,Acc) ->
    parse_zrangebyscore_options(T,Safe,['WITHSCORES'|Acc]);
parse_zrangebyscore_options([<<"LIMIT">>,Offset,Count|T],Safe,Acc) ->
    parse_zrangebyscore_options(T,Safe,[{'LIMIT',to_integer(Offset),to_integer(Count)}|Acc]).

parse_zstore_options(L,Safe) ->
    parse_zstore_options(L,Safe,keys,[],[],[]).

parse_zstore_options([],_Safe,keys,Acc1,[],[]) ->
    {lists:reverse(Acc1), []};
parse_zstore_options([],_Safe,weights,Acc1,Acc2,Acc3) ->
    {lists:reverse(Acc1), lists:reverse([{'WEIGHTS',lists:reverse(Acc2)}|Acc3])};
parse_zstore_options([],_Safe,aggregate,Acc1,Acc2,Acc3) ->
    {lists:reverse(Acc1), lists:reverse([{'AGGREGATE',Acc2}|Acc3])};
%% keys->weights
parse_zstore_options([<<"WEIGHTS">>|T],Safe,keys,Acc1,_Acc2,Acc3) ->
    parse_zstore_options(T,Safe,weights,Acc1,[],Acc3);
%% keys->aggregate
parse_zstore_options([<<"AGGREGATE">>|T],Safe,keys,Acc1,_Acc2,Acc3) ->
    parse_zstore_options(T,Safe,aggregate,Acc1,undefined,Acc3);
%% weights->weights
parse_zstore_options([<<"WEIGHTS">>|T],Safe,weights,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,weights,Acc1,[],[{'WEIGHTS',lists:reverse(Acc2)}|Acc3]);
%% weights->aggregate
parse_zstore_options([<<"AGGREGATE">>|T],Safe,weights,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,aggregate,Acc1,undefined,[{'WEIGHTS',lists:reverse(Acc2)}|Acc3]);
%% aggregate->weights
parse_zstore_options([<<"WEIGHTS">>|T],Safe,aggregate,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,weights,Acc1,[],[{'AGGREGATE',Acc2}|Acc3]);
%% aggregate->aggregate
parse_zstore_options([<<"AGGREGATE">>|T],Safe,aggregate,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,aggregate,Acc1,undefined,[{'AGGREGATE',Acc2}|Acc3]);
%% keys
parse_zstore_options([H|T],Safe,keys,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,keys,[H|Acc1],Acc2,Acc3);
%% weights
parse_zstore_options([H|T],Safe,weights,Acc1,Acc2,Acc3) ->
    parse_zstore_options(T,Safe,weights,Acc1,[to_number(H)|Acc2],Acc3);
%% aggregate
parse_zstore_options([H|T],Safe,aggregate,Acc1,undefined,Acc3) ->
    parse_zstore_options(T,Safe,aggregate,Acc1,to_atom(H,Safe),Acc3).

splitlast([]) ->
    erlang:error(badarg,[[]]);
splitlast(L) ->
    splitlast(L, []).

splitlast([H], Acc) ->
    {lists:reverse(Acc), H};
splitlast([H|T], Acc) ->
    splitlast(T, [H|Acc]).

identity(X) ->
    X.

to_atom(X,true) ->
    to_existing_atom(X);
to_atom(X,_) ->
    to_atom(X).

to_atom(X) ->
    list_to_atom(binary_to_list(X)).

to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).

to_integer(X) ->
    list_to_integer(binary_to_list(X)).

to_float(X) ->
    list_to_float(binary_to_list(X)).

to_number(X) ->
    list_to_number(binary_to_list(X)).

to_scoremin(<<$(,X/binary>>,_Safe) ->
    {'(', to_number(X)};
to_scoremin(X,Safe) ->
    try to_number(X)
    catch
        error:badarg ->
            to_atom(X,Safe)
    end.

to_scoremax(X,Safe) ->
    to_scoremin(X,Safe).

list_to_number(L) ->
    try list_to_float(L)
    catch
        error:badarg ->
            list_to_integer(L)
    end.

list_to_proplist(L) ->
    list_to_proplist(L, fun identity/1, fun identity/1).

list_to_proplist(L, FunK, FunV) ->
    list_to_proplist(L, FunK, FunV, []).

list_to_proplist([], _FunK, _FunV, Acc) ->
    lists:reverse(Acc);
list_to_proplist([K,V|T], FunK, FunV, Acc) ->
    list_to_proplist(T, FunK, FunV, [{FunK(K),FunV(V)}|Acc]).

from_integer(X) ->
    list_to_binary(integer_to_list(X)).

from_float(X) ->
    list_to_binary(float_to_list(X)).

from_atom(X) ->
    list_to_binary(atom_to_list(X)).
