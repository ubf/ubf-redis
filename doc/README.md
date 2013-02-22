

#Universal Binary Format and Redis#


Copyright (c) 2012-2013 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>This is UBF-REDIS, a framework for integrating UBF and the Redis
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

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



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




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><code>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</code></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><code>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</code></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/ubf/manifests.git -m ubf-redis-default.xml</code></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:ubf/manifests.git -m ubf-redis-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><code>$ cd working-directory-name
$ repo sync</code></pre>

</li>
</ol>
<p>For further information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B02 has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.8.0 has been tested most recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.2 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/rebar/rebar/wiki">https://github.com/rebar/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.html">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><code>$ cd working-directory-name
$ make compile</code></pre>

</li>
<li>
<p>
Run the unit tests
</p>


<pre><code>$ cd working-directory-name
$ make eunit</code></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><code>$ cd working-directory-name
$ make build-plt</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze</code></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><code>$ cd working-directory-name
$ make dialyze-nospec</code></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_to_test_quickcheck">To test - QuickCheck</h2>

<ol class="arabic">
<li>
<p>
Make sure QuickCheck is in your Erlang code path.  One simple way
   to accomplish this is by adding the code path to your <code>~/.erlang</code>
   resource file.
</p>


<pre><code>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/quviq/eqc-X.Y.Z/ebin").</code></pre>

</li>
<li>
<p>
Compile for QuickCheck
</p>


<pre><code>$ cd working-directory-name
$ make clean eqc</code></pre>

</li>
<li>
<p>
Run 5,000 Redis Encoder/Decoder QuickCheck tests
</p>


<pre><code>$ cd working-directory-name/deps/ubf_redis/.eqc
$ erl -smp +A 5 -pz ../../ubf/ebin -pz ../../qc/ebin

1> ruf_tests:qc_run(5000).
prop_ruf_requests: ...
...
Ok, passed 5000 tests
prop_ruf_responses: ...
...
Ok, passed 5000 tests
[]
.......</code></pre>

</li>
<li>
<p>
Run 500 Redis Proxy QuickCheck tests
</p>


<pre><code>$ cd working-directory-name/deps/ubf_redis/.eqc
$ erl -smp +A 5 -pz ../../ubf/ebin -pz ../../qc/ebin

1> redis_proxy_tests:qc_run(500).
#Port<0.646>: {data,{eol,<<"[9204] 28 Jul 23:29:49 # Opening port 6379: bind: Address already in use">>}}
#Port<0.663>: {data,{eol,<<"[9209] 28 Jul 23:29:49 * Server started, Redis version 2.4.15">>}}
#Port<0.663>: {data,{eol,<<"[9209] 28 Jul 23:29:49 * The server is now ready to accept connections on port 6379">>}}
....
OK, passed 500 tests

100.0% {1,attempts}

1.01% {decr_req,'->','integer()'}
1.01% {bitop_and_req,'->',{error,'ERR unknown command \'BITOP\''}}
0.98% {bitop_xor_req,'->',{error,'ERR unknown command \'BITOP\''}}
0.96% {srandmember_req,'->',undefined}
0.95% {hmset_req,'->',ok}
0.95% {bitop_not_req,'->',{error,'ERR unknown command \'BITOP\''}}
0.93% {zcount_req,'->','integer()'}
0.93% {zadd_req,'->','integer()'}
0.93% {msetnx_req,'->','integer()'}
0.92% {zscore_req,'->',undefined}
0.92% {zremrangebyrank_req,'->','integer()'}
0.92% {pexpire_req,'->',{error,'ERR unknown command \'PEXPIRE\''}}
0.92% {incr_req,'->','integer()'}
0.91% {scard_req,'->','integer()'}
0.91% {object_encoding_req,'->',undefined}
0.90% {bitop_or_req,'->',{error,'ERR unknown command \'BITOP\''}}
0.90% {append_req,'->','integer()'}
0.89% {time_req,'->',{error,'ERR unknown command \'TIME\''}}
0.89% {sinterstore_req,'->','integer()'}
0.89% {pttl_req,'->',{error,'ERR unknown command \'PTTL\''}}
0.88% {zrem_req,'->','integer()'}
0.88% {smembers_req,'->','list()'}
0.88% {move_req,'->',{error,'ERR index out of range'}}
0.88% {monitor_req,'->',ok}
0.88% {mget_req,'->','list()'}
0.88% {lindex_req,'->',undefined}
0.88% {info_req,'->','list()'}
0.88% {flushall_req,'->',ok}
0.88% {config_resetstat_req,'->',ok}
0.87% {zrevrank_req,'->',undefined}
0.87% {setex_req,'->',ok}
0.87% {rpushx_req,'->','integer()'}
0.87% {hincrbyfloat_req,'->',{error,'ERR unknown command \'HINCRBYFLOAT\''}}
0.87% {expireat_req,'->','integer()'}
0.87% {exists_req,'->','integer()'}
0.86% {sunionstore_req,'->','integer()'}
0.86% {strlen_req,'->','integer()'}
0.86% {renamenx_req,'->',{error,'ERR no such key'}}
0.86% {rename_req,'->',{error,'ERR no such key'}}
0.86% {keys_req,'->','list()'}
0.86% {bitcount_req,'->',{error,'ERR unknown command \'BITCOUNT\''}}
0.86% {sadd_req,'->','integer()'}
0.86% {mset_req,'->',ok}
0.86% {hlen_req,'->','integer()'}
0.86% {bgrewriteaof_req,'->',ok}
0.85% {sdiff_req,'->','list()'}
0.85% {getrange_req,'->','binary()'}
0.84% {slaveof_req,'->',ok}
0.84% {incrbyfloat_req,'->',{error,'ERR unknown command \'INCRBYFLOAT\''}}
0.84% {hsetnx_req,'->','integer()'}
0.84% {decrby_req,'->','integer()'}
0.83% {zcard_req,'->','integer()'}
0.83% {select_req,'->',{error,'ERR invalid DB index'}}
0.83% {getset_req,'->',undefined}
0.82% {zremrangebyscore_req,'->','integer()'}
0.82% {srem_req,'->','integer()'}
0.82% {sismember_req,'->','integer()'}
0.82% {restore_req,'->',{error,'ERR unknown command \'RESTORE\''}}
0.82% {pexpireat_req,'->',{error,'ERR unknown command \'PEXPIREAT\''}}
0.82% {incrby_req,'->','integer()'}
0.82% {sdiffstore_req,'->','integer()'}
0.82% {ltrim_req,'->',ok}
0.82% {lset_req,'->',{error,'ERR no such key'}}
0.82% {lpush_req,'->','integer()'}
0.82% {del_req,'->','integer()'}
0.81% {type_req,'->',none}
0.81% {multi_req,'->',ok}
0.81% {lrem_req,'->','integer()'}
0.81% {hexists_req,'->','integer()'}
0.81% {debug_object_req,'->',{error,'ERR no such key'}}
0.80% {smove_req,'->','integer()'}
0.80% {object_idletime_req,'->',undefined}
0.80% {linsert_req,'->','integer()'}
0.80% {get_req,'->',undefined}
0.80% {flushdb_req,'->',ok}
0.79% {zrangebyscore_req,'->','list()'}
0.79% {watch_req,'->',ok}
0.79% {migrate_req,'->',{error,'ERR unknown command \'MIGRATE\''}}
0.78% {zrevrangebyscore_req,'->','list()'}
0.78% {zrank_req,'->',undefined}
0.78% {set_req,'->',ok}
0.78% {lpushx_req,'->','integer()'}
0.78% {config_get_req,'->','list()'}
0.78% {lpop_req,'->',undefined}
0.78% {hvals_req,'->','list()'}
0.78% {hgetall_req,'->','list()'}
0.77% {rpush_req,'->','integer()'}
0.77% {psetex_req,'->',{error,'ERR unknown command \'PSETEX\''}}
0.77% {dump_req,'->',{error,'ERR unknown command \'DUMP\''}}
0.76% {spop_req,'->',undefined}
0.76% {hincrby_req,'->','integer()'}
0.76% {hget_req,'->',undefined}
0.76% {discard_req,'->',{error,'ERR DISCARD without MULTI'}}
0.75% {llen_req,'->','integer()'}
0.75% {expire_req,'->','integer()'}
0.74% {zrevrange_req,'->',{error,'ERR syntax error'}}
0.74% {lrange_req,'->','list()'}
0.74% {hset_req,'->','integer()'}
0.74% {getbit_req,'->','integer()'}
0.74% {hdel_req,'->','integer()'}
0.73% {sunion_req,'->','list()'}
0.73% {hmget_req,'->','list()'}
0.73% {hkeys_req,'->','list()'}
0.73% {config_set_req,'->','error()'}
0.72% {setnx_req,'->','integer()'}
0.72% {rpop_req,'->',undefined}
0.72% {persist_req,'->','integer()'}
0.72% {lastsave_req,'->','integer()'}
0.72% {dbsize_req,'->','integer()'}
0.71% {setbit_req,'->',{error,'ERR bit is not an integer or out of range'}}
0.71% {auth_req,'->',{error,'ERR Client sent AUTH, but no password is set'}}
0.70% {sinter_req,'->','list()'}
0.70% {ping_req,'->',ok}
0.70% {object_refcount_req,'->',undefined}
0.69% {zrange_req,'->',{error,'ERR syntax error'}}
0.68% {sort_req,'->','list()'}
0.65% {ttl_req,'->','integer()'}
0.63% {rpoplpush_req,'->',undefined}
0.63% {echo_req,'->','binary()'}
0.61% {setrange_req,'->',{error,'ERR string exceeds maximum allowed size (512MB)'}}
0.55% {randomkey_req,'->','binary()'}
0.44% {zincrby_req,'->','integer()'}
0.23% {zincrby_req,'->','float()'}
0.15% {randomkey_req,'->',undefined}
0.14% {setrange_req,'->','integer()'}
0.10% {zrevrange_req,'->','list()'}
0.10% {sort_req,'->','integer()'}
0.09% {zrange_req,'->','list()'}
0.06% {setbit_req,'->','integer()'}
0.02% {sdiff_req,'->',{error,'ERR Operation against a key holding the wrong kind of value'}}
0.02% {bgrewriteaof_req,'->',{error,'ERR Background append only file rewriting already in progress'}}
0.01% {sunion_req,'->',{error,'ERR Operation against a key holding the wrong kind of value'}}
0.01% {sdiffstore_req,'->',{error,'ERR Operation against a key holding the wrong kind of value'}}
.......</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">This test requires redis-server to be installed under the
<code>/usr/local/</code> directory.</td>
</tr></table>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">This test removes all data under the
<code>/usr/local/var/db/redis/</code> directory.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - PropEr</h2>

<p><em>Under Construction - To Be Added</em></p>



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




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="ruf.md" class="module">ruf</a></td></tr>
<tr><td><a href="ruf_driver.md" class="module">ruf_driver</a></td></tr>
<tr><td><a href="ruf_term.md" class="module">ruf_term</a></td></tr>
<tr><td><a href="ubf_redis_plugin.md" class="module">ubf_redis_plugin</a></td></tr>
<tr><td><a href="ubf_redis_types_plugin.md" class="module">ubf_redis_types_plugin</a></td></tr></table>

