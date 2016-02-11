

# Universal Binary Format and Redis #

Copyright (c) 2012-2016 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>This is UBF-REDIS, a framework for integrating UBF and the Redis
Unified Format (RUF) protocol.  This repository depends on the ubf
open source repository.</p>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find UBF-REDIS useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download, build, and test the ubf_redis application in one shot,
please follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/ubf/ubf-redis.git ubf_redis
$ cd ubf_redis
$ make deps clean compile test</code></pre>




<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is a good first step.</p>
<p>The UBF User's Guide is the best next step.  Check out
<a href="http://ubf.github.com/ubf/ubf-user-guide.en.html">http://ubf.github.com/ubf/ubf-user-guide.en.html</a> for further
detailed information.</p>
<p>This repository has building blocks for constructing your own Redis
server clone in Erlang.</p>
<ul>
<li>
<p>
<code>src/ruf.erl</code> Redis network protocol (a.k.a. RUF) encoder/decoder
</p>
</li>
<li>
<p>
<code>src/ruf_term.erl</code> Redis <em>raw</em> Erlang encoder/decoder
</p>
</li>
<li>
<p>
<code>src/ubf_redis_types_plugin.erl</code> Erlang types for Redis RPC
</p>
</li>
<li>
<p>
<code>src/ubf_redis_plugin.erl</code> Erlang Redis RPC request/response pairs
</p>
</li>
</ul>
<p>The QC (a.k.a. QuickCheck, PropEr, etc.) tests underneath the
"tests/qc" directory should be helpful for understanding the
specification and behavior of these building blocks.</p>
<ul>
<li>
<p>
<code>test/qc/ruf_tests.erl prop_ruf_requests()</code> tests encoding and
  decoding of RUF requests.  Test inputs are <em>automagically</em> generated
  using the above Redis RPC types.<em>This QC test has been completed.</em>
</p>
</li>
<li>
<p>
<code>test/qc/ruf_tests.erl prop_ruf_responses()</code> tests encoding and
  decoding of RUF responses.  Test inputs are <em>automagically</em>
  generated using the above Redis RPC types.<em>This QC test has been
  completed.</em>
</p>
</li>
<li>
<p>
<code>test/qc/redis_proxy_tests.erl</code> tests an Erlang Redis client talking
  with an Erlang Redis "Proxy" server.  The Redis "Proxy" server
  forwards all requests transparently to a "real" Redis server (using
  another internal Erlang Redis client).  All requests and responses
  sent to the Redis "Proxy" server are <em>automagically</em> generated and
  checked using UBF's contract manager with the above Redis RPC types
  and request/response pairs.<em>This QC test is stil in-progress.</em>
</p>
</li>
</ul>
<p>A few "hello world" Eunit tests can also be found in the test/eunit
directory.</p>
<p>See below for further details on how to run these tests.</p>


<h3 id="_what_is_ubf">What is UBF?</h3>
<p>UBF is the "Universal Binary Format", designed and implemented by Joe
Armstrong.  UBF is a language for transporting and describing complex
data structures across a network.  It has three components:</p>
<ul>
<li>
<p>
UBF(a) is a "language neutral" data transport format, roughly
  equivalent to well-formed XML.
</p>
</li>
<li>
<p>
UBF(b) is a programming language for describing types in UBF(a) and
  protocols between clients and servers.  This layer is typically
  called the "protocol contract".  UBF(b) is roughly equivalent to
  Verified XML, XML-schemas, SOAP and WDSL.
</p>
</li>
<li>
<p>
UBF(c) is a meta-level protocol used between a UBF client and a UBF
  server.
</p>
</li>
</ul>
<p>See <a href="http://ubf.github.com/ubf">http://ubf.github.com/ubf</a> for further details.</p>


<h3 id="_what_is_redis">What is Redis?</h3>
<p>Redis is an open source, advanced key-value store. See <a href="http://redis.io">http://redis.io</a>
for full details. In particular, see <a href="http://redis.io/topics/protocol">http://redis.io/topics/protocol</a>
for a description of the Redis protocol.</p>


<h3 id="_tools">Tools</h3>
<p>For further information and help for related tools, please refer to
the following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R15B01 or newer, 17.0 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.9.3 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
</ul>




<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
Documentation
</p>
<ul>
<li>
<p>
Explain overall implementation and test strategy.
</p>
</li>
<li>
<p>
Explain relevant UBF features.
</p>
</li>
<li>
<p>
Explain how one can start to build their own Redis server in
    Erlang.
</p>
</li>
</ul>
</li>
<li>
<p>
Testing - Black Box
</p>
<ul>
<li>
<p>
Implement real "statem" test model for the Redis Proxy.
</p>
</li>
</ul>
</li>
</ul>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ruf.md" class="module">ruf</a></td></tr>
<tr><td><a href="ruf_driver.md" class="module">ruf_driver</a></td></tr>
<tr><td><a href="ruf_term.md" class="module">ruf_term</a></td></tr>
<tr><td><a href="ubf_redis_plugin.md" class="module">ubf_redis_plugin</a></td></tr>
<tr><td><a href="ubf_redis_types_plugin.md" class="module">ubf_redis_types_plugin</a></td></tr></table>

