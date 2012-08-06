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

-module(ruf).
-behaviour(contract_proto).

%%% ---------------------------------------------------
%%% @doc Redis Unified Format
%%%
%%% This module implements a decoder and encoder for the Redis Network
%%% Protocol.  See http://redis.io/topics/protocol for further
%%% details.
%%%
%%% Requests::
%%%   - Redis Unified request protocol
%%% +
%%% -------
%%% *<number of arguments> CR LF
%%% $<number of bytes of argument 1> CR LF
%%% <argument data> CR LF
%%% . . .
%%% $<number of bytes of argument N> CR LF
%%% <argument data> CR LF
%%% -------
%%% +
%%%   - Erlang (sans CR LF)
%%% +
%%% ------
%%% [binary()]+
%%% ------
%%%
%%% Responses::
%%%   Status reply;;
%%%     - Redis
%%% +
%%% ------
%%% +<reply data> CR LF
%%% ------
%%% +
%%%     - Erlang (sans CR LF)
%%% +
%%% ------
%%% {ok, binary()}
%%% ------
%%% +
%%%   Error reply;;
%%%     - Redis
%%% +
%%% ------
%%% +<reply data> CR LF
%%% ------
%%% +
%%%     - Erlang (sans CR LF)
%%% +
%%% ------
%%% {error, binary()}
%%% ------
%%% +
%%%   Integer reply;;
%%%     - Redis
%%% +
%%% ------
%%% :<integer data> CR LF
%%% ------
%%% +
%%%     - Erlang (sans CR LF)
%%% +
%%% ------
%%% integer()
%%% ------
%%% +
%%%   Bulk reply;;
%%%     - Redis
%%% +
%%% ------
%%% $<number of bytes of reply data> CR LF
%%% <reply data> CR LF
%%% ------
%%% +
%%%     - Erlang (sans CR LF)
%%% +
%%% ------
%%% binary()
%%% ------
%%% +
%%%   Multi-bulk reply;;
%%%    - Redis
%%% +
%%% -------
%%% *<number of replies> CR LF
%%% $<number of bytes of reply data 1> CR LF
%%% <reply data> CR LF
%%% . . .
%%% $<number of bytes of reply data N> CR LF
%%% <reply data> CR LF
%%% -------
%%% +
%%%     - Erlang (sans CR LF)
%%% +
%%% ------
%%% [binary()]
%%% ------
%%%
%%% @end
%%% ---------------------------------------------------

-include_lib("ubf/include/ubf.hrl").

%% API
-export([proto_vsn/0, proto_driver/0, proto_packet_type/0]).
-export([encode/1, encode/2, encode/3]).
-export([decode_init/0, decode_init/1, decode_init/2, decode/1, decode/2, decode/3]).

-export([atom_to_binary/1]).
-export([binary_to_atom/1, binary_to_existing_atom/1]).

-export_type([ruf/0]).

%% Dummy hack/kludge.
-export([contract_records/0]).

contract_records() ->
    [].

%%%=========================================================================
%%%  Records, Types, Macros
%%%=========================================================================

-record(state,
        {
          x     :: binary(),              % current binary to be decoded
          stack :: undefined | term(),    % current stack
          size  :: undefined | integer(), % current size (optional)
          last  :: undefined | integer(), % last (optional)
          safe  :: boolean(),             % safe
          vsn   :: undefined | string(),  % version
          mod   :: atom()                 % contract
        }
       ).

-type ruf() :: {ok, binary()} | {error, binary()} | integer() | binary() | [binary()].
-type ok() :: {ok, Output::ruf(), Remainder::binary(), VSN::string()}.
-type error() :: {error, Reason::term()}.
-type cont() :: cont1() | cont2().
-type cont1() :: {more, fun()}.
-type cont2() :: {more, fun(), #state{}}.

%%%=========================================================================
%%%  API
%%%=========================================================================

%%
%% @TODO
%%
%% - Implement nested multi-bulk replies for EXEC
%% - Test random pause/resume for ruf encoding and decoding
%% - Is there any solution for dealing with "untagged" responses
%%   (i.e. proplists, floats, atoms, etc. being returned as binaries
%%   or list of binaries rather than the corresponding Erlang term.
%%   This is one aspect of the Redis network protocol that I'm not
%%   very fond of it.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
proto_vsn()         -> 'ruf2.4.15.0'.
proto_driver()      -> ruf_driver.
proto_packet_type() -> 0.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
-spec encode(Input::ruf()) -> iolist() | no_return().
encode(X) ->
    encode(X, ?MODULE).

-spec encode(Input::ruf(), module()) -> iolist() | no_return().
encode(X, Mod) ->
    encode(X, Mod, undefined).

-spec encode(Input::ruf(), module(), VSN::undefined | string()) -> iolist() | no_return().
encode(X, Mod, VSN) ->
    encode_message(X, Mod, VSN).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
-spec decode_init() -> cont2().
decode_init() ->
    decode_init(false).

-spec decode_init(Safe::boolean()) -> cont2().
decode_init(Safe) ->
    decode_init(Safe, <<>>).

-spec decode_init(Safe::boolean(), Input::binary()) -> cont2().
decode_init(Safe, Binary) ->
    {more, fun decode_start/1, #state{x=Binary, safe=Safe}}.

-spec decode(Input::binary()) -> ok() | error() | cont1().
decode(X) ->
    decode(X, ?MODULE).

-spec decode(Input::binary(), module()) -> ok() | error() | cont1().
decode(X, Mod) ->
    decode(X, Mod, decode_init()).

-spec decode(Input::binary(), module(), cont()) -> ok() | error() | cont1().
decode(X, Mod, {more, Fun}) ->
    Fun(#state{x=X,mod=Mod});
decode(X, Mod, {more, Fun, #state{x=Old}=State}) ->
    Fun(State#state{x= <<Old/binary, X/binary>>, mod=Mod}).

decode_start(S) ->
    decode_message(S, fun decode_finish/1).

decode_finish(#state{x=X,stack=Term,vsn=VSN}) ->
    {ok, Term, X, VSN}.

decode_pause(#state{x=X}=S, Cont, Resume) ->
    {more, fun(#state{x=X1,mod=Mod1}) ->
                   Resume(S#state{x= <<X/binary,X1/binary>>,mod=Mod1}, Cont)
           end}.

decode_error(Type, S) ->
    {error, {Type, S}}.

%%%========================================================================
%%% Internal functions
%%%========================================================================

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_message(X, _Mod, _VSN) ->
    case get('ubf_info') of
        ruf_client_driver ->
            encode_request(X);
        ruf_driver ->
            case X of
                {ok, X1} when is_binary(X1) ->
                    encode_response_ok(X1);
                {error, X1} when is_binary(X1) ->
                    encode_response_error(X1);
                X when is_integer(X) ->
                    encode_response_integer(X);
                X when is_binary(X); X==undefined ->
                    encode_response_bulk(X);
                X when is_list(X) ->
                    encode_response_multibulk(X)
            end;
        I ->
            error(badarg, [I])
    end.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------

encode_request(X) when not is_list(X) ->
    [$*, "1", $\r, $\n, encode_response_bulk(X)];
encode_request(X) ->
    encode_request(lists:flatten(X),0,[]).

encode_request([],Len,Acc) ->
    [$*, integer_to_list(Len), $\r, $\n, lists:reverse(Acc)];
encode_request([H|T],Len,Acc) ->
    encode_request(T,Len+1,[encode_response_bulk(H)|Acc]).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_response_ok(X) ->
    [$+, X, $\r, $\n].

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_response_error(X) ->
    [$-, X, $\r, $\n].

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_response_integer(X) ->
    [$:, integer_to_list(X), $\r, $\n].

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_response_bulk(undefined) ->
    [$$, "-1", $\r, $\n];
encode_response_bulk(X) ->
    [$$, integer_to_list(iolist_size(X)), $\r, $\n, X, $\r, $\n].

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
encode_response_multibulk(X) ->
    encode_response_multibulk(X,0,[]).

encode_response_multibulk([],Len,Acc) ->
    [$*, integer_to_list(Len), $\r, $\n, lists:reverse(Acc)];
encode_response_multibulk([H|T],Len,Acc) ->
    encode_response_multibulk(T,Len+1,[encode_response_bulk(H)|Acc]).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_message(#state{x=X,stack=undefined}=S, Cont) ->
    case get('ubf_info') of
        ruf_client_driver ->
            case X of
                <<$+,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(response_ok, S1, Cont) end,
                    decode_response_ok(S#state{x=X1,stack=[[]]}, Cont1);
                <<$-,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(response_error, S1, Cont) end,
                    decode_response_error(S#state{x=X1,stack=[[]]}, Cont1);
                <<$:,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(response_integer, S1, Cont) end,
                    decode_response_integer(S#state{x=X1,stack=[[]]}, Cont1);
                <<$$,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(response_bulk, S1, Cont) end,
                    decode_response_bulk(S#state{x=X1,stack=[[]]}, Cont1);
                <<$*,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(response_multibulk, S1, Cont) end,
                    decode_response_multibulk(S#state{x=X1,stack=[[]]}, Cont1);
                <<>> ->
                    decode_pause(S, Cont, fun decode_message/2);
                _ ->
                    decode_error(message, S)
            end;
        ruf_driver ->
            case X of
                <<$*,X1/binary>> ->
                    Cont1 = fun(S1) -> decode_message_finish(request, S1, Cont) end,
                    decode_request(S#state{x=X1,stack=[[]]}, Cont1);
                <<>> ->
                    decode_pause(S, Cont, fun decode_message/2);
                _ ->
                    decode_error(message, S)
            end;
        I ->
            error(badarg, [I])
    end.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_message_finish(request, #state{stack=[X]}=S, Cont) ->
    Cont(S#state{stack=X});
decode_message_finish(response_ok, #state{stack=[X,[]]}=S, Cont) ->
    Cont(S#state{stack={ok, X}});
decode_message_finish(response_error, #state{stack=[X,[]]}=S, Cont) ->
    Cont(S#state{stack={error, X}});
decode_message_finish(response_integer, #state{stack=[X,[]]}=S, Cont) ->
    Cont(S#state{stack=X});
decode_message_finish(response_bulk, #state{stack=[X,[]]}=S, Cont) ->
    Cont(S#state{stack=X});
decode_message_finish(response_multibulk, #state{stack=[X]}=S, Cont) ->
    Cont(S#state{stack=X}).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_request(S, Cont) ->
    decode_multibulk(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_response_ok(S, Cont) ->
    decode_line(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_response_error(S, Cont) ->
    decode_line(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_response_integer(S, Cont) ->
    decode_line_integer(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_response_bulk(S, Cont) ->
    decode_bulk(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_response_multibulk(S, Cont) ->
    decode_multibulk(S, Cont).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_bulk(#state{x=X}=S, Cont) ->
    Cont1 = fun(S1) -> decode_line_max(S1, Cont) end,
    decode_line_non_neg_integer_or_nil(S#state{x=X}, Cont1).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_multibulk(S, Cont) ->
    Cont1 = fun(S1) -> decode_multibulk_body(S1, Cont) end,
    decode_line_non_neg_integer(S, Cont1).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_multibulk_body(#state{stack=[0|Stack]}=S, Cont) ->
    Cont(S#state{stack=Stack});
decode_multibulk_body(#state{stack=[N|Stack]}=S, Cont) ->
    Cont1 = fun(S1) -> decode_bulkN(N-1, [], S1, Cont) end,
    decode_bulk_body(S#state{stack=[Stack]}, Cont1).

decode_bulkN(0, Acc, #state{stack=[H|_Stack]}=S, Cont) ->
    Cont(S#state{stack=[lists:reverse([H|Acc])]});
decode_bulkN(N, Acc, #state{stack=[H|Stack]}=S, Cont) when N > 0 ->
    Cont1 = fun(S1) -> decode_bulkN(N-1, [H|Acc], S1, Cont) end,
    decode_bulk_body(S#state{stack=[Stack]}, Cont1).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_bulk_body(#state{x=X}=S, Cont) ->
    case X of
        <<$$,X1/binary>> ->
            Cont1 = fun(S1) -> decode_line_max(S1, Cont) end,
            decode_line_non_neg_integer_or_nil(S#state{x=X1}, Cont1);
        <<>> ->
            decode_pause(S, Cont, fun decode_bulk_body/2);
        _ ->
            decode_error(bulk, S)
    end.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_line_integer(S, Cont) ->
    Cont1 = fun(S1) -> decode_integer(S1, Cont) end,
    decode_line(S, Cont1).

decode_integer(#state{stack=[H|Stack]}=S, Cont) ->
    I = binary_to_integer(H),
    Cont(S#state{stack=[I|Stack]}).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_line_non_neg_integer(S, Cont) ->
    Cont1 = fun(S1) -> decode_non_neg_integer(S1, Cont) end,
    decode_line(S, Cont1).

decode_non_neg_integer(#state{stack=[H|Stack]}=S, Cont) ->
    I = binary_to_non_neg_integer(H),
    Cont(S#state{stack=[I|Stack]}).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_line_non_neg_integer_or_nil(S, Cont) ->
    Cont1 = fun(S1) -> decode_non_neg_integer_or_nil(S1, Cont) end,
    decode_line(S, Cont1).

decode_non_neg_integer_or_nil(#state{stack=[H|Stack]}=S, Cont) ->
    I = case binary_to_integer(H) of
            X when X >= 0 ->
                X;
            -1 ->
                undefined
        end,
    Cont(S#state{stack=[I|Stack]}).

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_line(#state{x=X,stack=Stack}=S, Cont) ->
    decode_line_loop(S#state{x=X,size=0,last=undefined,stack=[[X]|Stack]}, Cont).

decode_line_resume(#state{x=X,stack=Stack}=S, Cont) ->
    decode_line_loop(S#state{x=X,stack=push(X, Stack)}, Cont).

decode_line_loop(#state{x=X,size=Size,last=Last,stack=[Line|Stack]}=S, Cont) ->
    case X of
        <<$\n,X1/binary>> when Last == <<$\r>> ->
            Size1 = Size - 1,
            <<Line1:Size1/binary,$\r,_/binary>> = iolist_to_binary(lists:reverse(Line)),
            Cont(S#state{x=X1, stack=[Line1|Stack]});
        <<Last1:1/binary,X1/binary>> ->
            decode_line_loop(S#state{x=X1,size=Size+1,last=Last1}, Cont);
        <<>> ->
            decode_pause(S, Cont, fun decode_line_resume/2)
    end.

%% -----------------------------------------------------------------
%%
%% -----------------------------------------------------------------
decode_line_max(#state{stack=[undefined|_Stack]}=S, Cont) ->
    Cont(S);
decode_line_max(#state{stack=[Max|Stack]}=S, Cont) ->
    decode_line_max_loop(S#state{size=Max+2,stack=[Stack]}, Cont).

decode_line_max_loop(#state{x=X,size=Size,stack=[Stack]}=S, Cont) when byte_size(X) >= Size ->
    Size1 = Size - 2,
    <<Line:Size1/binary,$\r,$\n,X1/binary>> = X,
    Cont(S#state{x=X1,stack=[Line|Stack]});
decode_line_max_loop(S, Cont) ->
    decode_pause(S, Cont, fun decode_line_max_loop/2).

%%%========================================================================
%%% Utility functions
%%%========================================================================

atom_to_binary(X) ->
    list_to_binary(atom_to_list(X)).

binary_to_atom(X) ->
    list_to_atom(binary_to_list(X)).

binary_to_existing_atom(X) ->
    list_to_existing_atom(binary_to_list(X)).

binary_to_integer(X) ->
    list_to_integer(binary_to_list(X)).

binary_to_non_neg_integer(X) ->
    case binary_to_integer(X) of
        Y when Y >= 0 ->
            Y
    end.

push(X, [Top|Rest]) ->
    [[X|Top]|Rest].
