

#Module ruf#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


__Behaviours:__ [`contract_proto`](https://github.com/ubf/ubf/blob/master/doc/contract_proto.md).
<a name="types"></a>

##Data Types##




###<a name="type-cont">cont()</a>##



<pre>cont() = <a href="#type-cont1">cont1()</a> | <a href="#type-cont2">cont2()</a></pre>



###<a name="type-cont1">cont1()</a>##



<pre>cont1() = {more, function()}</pre>



###<a name="type-cont2">cont2()</a>##



<pre>cont2() = {more, function(), #state{}}</pre>



###<a name="type-error">error()</a>##



<pre>error() = {error, Reason::term()}</pre>



###<a name="type-ok">ok()</a>##



<pre>ok() = {ok, Output::<a href="#type-ruf">ruf()</a>, Remainder::binary(), VSN::string()}</pre>



###<a name="type-ruf">ruf()</a>##



<pre>ruf() = {ok, binary()} | {error, binary()} | integer() | binary() | [binary()]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom_to_binary-1">atom_to_binary/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_atom-1">binary_to_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#binary_to_existing_atom-1">binary_to_existing_atom/1</a></td><td></td></tr><tr><td valign="top"><a href="#contract_records-0">contract_records/0</a></td><td><p>Redis Unified Format</p>


<pre><tt>This module implements a decoder and encoder for the Redis Network
Protocol.  See http://redis.io/topics/protocol for further
details.</tt></pre>

<dl>
<dt class="hdlist1">
Requests
</dt>
<dd>
<ul>
<li>
<p>
Redis Unified request protocol
<br>
-------
*<number of arguments> CR LF
$<number of bytes of argument 1> CR LF
<argument data> CR LF
</p>
<ol class="arabic">
<li>
<p>
. .
$<number of bytes of argument N> CR LF
<argument data> CR LF
-------
<br>
</p>
</li>
</ol>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
[binary()]+
------
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Responses
</dt>
<dd>
<dl>
<dt class="hdlist1">
Status reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
+<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
{ok, binary()}
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Error reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
+<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
{error, binary()}
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Integer reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
:<integer data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
integer()
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Bulk reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
$<number of bytes of reply data> CR LF
<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
binary()
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Multi-bulk reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
-------
*<number of replies> CR LF
$<number of bytes of reply data 1> CR LF
<reply data> CR LF
</p>
<ol class="arabic">
<li>
<p>
. .
$<number of bytes of reply data N> CR LF
<reply data> CR LF
-------
<br>
</p>
</li>
</ol>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
[binary()]
------
</p>
</li>
</ul>
</dd>
</dl>
</dd>
</dl>.</td></tr><tr><td valign="top"><a href="#decode-1">decode/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td></td></tr><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-0">decode_init/0</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-1">decode_init/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_init-2">decode_init/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-1">encode/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td></td></tr><tr><td valign="top"><a href="#encode-3">encode/3</a></td><td></td></tr><tr><td valign="top"><a href="#proto_driver-0">proto_driver/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_packet_type-0">proto_packet_type/0</a></td><td></td></tr><tr><td valign="top"><a href="#proto_vsn-0">proto_vsn/0</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="atom_to_binary-1"></a>

###atom_to_binary/1##


`atom_to_binary(X) -> any()`

<a name="binary_to_atom-1"></a>

###binary_to_atom/1##


`binary_to_atom(X) -> any()`

<a name="binary_to_existing_atom-1"></a>

###binary_to_existing_atom/1##


`binary_to_existing_atom(X) -> any()`

<a name="contract_records-0"></a>

###contract_records/0##


`contract_records() -> any()`

<p>Redis Unified Format</p>


<pre><tt>This module implements a decoder and encoder for the Redis Network
Protocol.  See http://redis.io/topics/protocol for further
details.</tt></pre>

<dl>
<dt class="hdlist1">
Requests
</dt>
<dd>
<ul>
<li>
<p>
Redis Unified request protocol
<br>
-------
*<number of arguments> CR LF
$<number of bytes of argument 1> CR LF
<argument data> CR LF
</p>
<ol class="arabic">
<li>
<p>
. .
$<number of bytes of argument N> CR LF
<argument data> CR LF
-------
<br>
</p>
</li>
</ol>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
[binary()]+
------
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Responses
</dt>
<dd>
<dl>
<dt class="hdlist1">
Status reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
+<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
{ok, binary()}
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Error reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
+<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
{error, binary()}
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Integer reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
:<integer data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
integer()
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Bulk reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
------
$<number of bytes of reply data> CR LF
<reply data> CR LF
------
<br>
</p>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
binary()
------
<br>
</p>
</li>
</ul>
</dd>
<dt class="hdlist1">
Multi-bulk reply
</dt>
<dd>
<ul>
<li>
<p>
Redis
<br>
-------
*<number of replies> CR LF
$<number of bytes of reply data 1> CR LF
<reply data> CR LF
</p>
<ol class="arabic">
<li>
<p>
. .
$<number of bytes of reply data N> CR LF
<reply data> CR LF
-------
<br>
</p>
</li>
</ol>
</li>
<li>
<p>
Erlang (sans CR LF)
<br>
------
[binary()]
------
</p>
</li>
</ul>
</dd>
</dl>
</dd>
</dl>
<a name="decode-1"></a>

###decode/1##


<pre>decode(Input::binary()) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont1">cont1()</a></pre>
<br></br>


<a name="decode-2"></a>

###decode/2##


<pre>decode(Input::binary(), Mod::module()) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont1">cont1()</a></pre>
<br></br>


<a name="decode-3"></a>

###decode/3##


<pre>decode(Input::binary(), Mod::module(), X3::<a href="#type-cont">cont()</a>) -> <a href="#type-ok">ok()</a> | <a href="#type-error">error()</a> | <a href="#type-cont1">cont1()</a></pre>
<br></br>


<a name="decode_init-0"></a>

###decode_init/0##


<pre>decode_init() -> <a href="#type-cont2">cont2()</a></pre>
<br></br>


<a name="decode_init-1"></a>

###decode_init/1##


<pre>decode_init(Safe::boolean()) -> <a href="#type-cont2">cont2()</a></pre>
<br></br>


<a name="decode_init-2"></a>

###decode_init/2##


<pre>decode_init(Safe::boolean(), Input::binary()) -> <a href="#type-cont2">cont2()</a></pre>
<br></br>


<a name="encode-1"></a>

###encode/1##


<pre>encode(Input::<a href="#type-ruf">ruf()</a>) -> iolist() | no_return()</pre>
<br></br>


<a name="encode-2"></a>

###encode/2##


<pre>encode(Input::<a href="#type-ruf">ruf()</a>, Mod::module()) -> iolist() | no_return()</pre>
<br></br>


<a name="encode-3"></a>

###encode/3##


<pre>encode(Input::<a href="#type-ruf">ruf()</a>, Mod::module(), VSN::undefined | string()) -> iolist() | no_return()</pre>
<br></br>


<a name="proto_driver-0"></a>

###proto_driver/0##


`proto_driver() -> any()`

<a name="proto_packet_type-0"></a>

###proto_packet_type/0##


`proto_packet_type() -> any()`

<a name="proto_vsn-0"></a>

###proto_vsn/0##


`proto_vsn() -> any()`
