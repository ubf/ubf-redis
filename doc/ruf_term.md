

# Module ruf_term #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-ruf">ruf()</a> ###



<pre><code>
ruf() = <a href="ruf.md#type-ruf">ruf:ruf()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decode-3">decode/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode-4">encode/4</a></td><td><p>Redis Unified Format - Erlang Term Format</p>


<pre><code>This module implements a decoder and encoder between Redis
Unified Format and Erlang Term Format.</code></pre>



<pre><code>NOTE: This module acts as bridge between Redis and Erlang
conventions for requests and responses.</code></pre>
.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decode-3"></a>

### decode/3 ###


<pre><code>
decode(Input::term(), Contract::module(), VSN::undefined | string()) -&gt; <a href="#type-ruf">ruf()</a> | no_return()
</code></pre>

<br></br>



<a name="encode-4"></a>

### encode/4 ###


<pre><code>
encode(Input::<a href="#type-ruf">ruf()</a>, Contract::module(), VSN::undefined | string(), Safe::boolean()) -&gt; term() | no_return()
</code></pre>

<br></br>


<p>Redis Unified Format - Erlang Term Format</p>


<pre><code>This module implements a decoder and encoder between Redis
Unified Format and Erlang Term Format.</code></pre>



<pre><code>NOTE: This module acts as bridge between Redis and Erlang
conventions for requests and responses.</code></pre>


