

# Module hdr_iter #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


.
Copyright (c) 2014 Darach Ennis

__Version:__ 0.2.0


__Authors:__ Darach Ennis ([`darach@gmail.com`](mailto:darach@gmail.com)).
<a name="description"></a>

## Description ##

The HDR histogram library is an Erlang native interface function wrapper of
Mike Barker's C port of Gil Tene's HDR Histogram utility.

A high dynamic range histogram is one that supports recording and analyzing
sampled data points across a configurable range with configurable precision
within that range. The precision is expressed as a number of significant
figures in the recording.



This HDR histogram implementation is designed for recording histograms of
value measurements in latency sensitive environments. Although the native
recording times can be as low as single digit nanoseconds there is added
overhead in this wrapper/binding due to both the frontend overhead of converting
from native C to the NIF interface, and the erlang overhead incurred calling
into the NIFs. C'est la vie, I suppose.



A distinct advantage of this histogram implementation is constant space and
recording (time) overhead with an ability to recycle and reset instances whilst
reclaiming already allocated space for reuse thereby reducing allocation cost
and garbage collection overhead in the BEAM where repeated or continuous usage
is likely. For example, a gen_server recording metrics continuously and resetting
and logging histogram dumps on a periodic or other windowed basis.



The code is released to the public domain, under the same terms as its
sibling projects, as explained in the LICENSE.txt and COPYING.txt in the
root of this repository, but normatively at:



http://creativecommons.org/publicdomain/zero/1.0/



For users of this code who wish to consume it under the "BSD" license
rather than under the public domain or CC0 contribution text mentioned
above, the code found under this directory is *also* provided under the
following license (commonly referred to as the BSD 2-Clause License). This
license does not detract from the above stated release of the code into
the public domain, and simply represents an additional license granted by
http://creativecommons.org/publicdomain/zero/1.0/



-----------------------------------------------------------------------------
** Beginning of "BSD 2-Clause License" text. **



Copyright (c) 2012, 2013, 2014 Gil Tene
Copyright (c) 2014 Michael Barker
Copyright (c) 2014 Darach Ennis
All rights reserved.



Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:



1. Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.



2. Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
THE POSSIBILITY OF SUCH DAMAGE.

<a name="types"></a>

## Data Types ##




### <a name="type-ref">ref()</a> ###



<pre><code>
ref() = binary()
</code></pre>



  NIF private data (looks like empty binary)
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close this HDR histogram instance and free any system resources.</td></tr><tr><td valign="top"><a href="#each-2">each/2</a></td><td>Iterate over histogram applying a function to each data point.</td></tr><tr><td valign="top"><a href="#each-3">each/3</a></td><td>Iterate over histogram applying a function to each data point accumulating a result.</td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td>Open a fresh instance of a high dynamic range (HDR) histogram iterator.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Ref) -&gt; ok | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li></ul>

Close this HDR histogram instance and free any system resources
<a name="each-2"></a>

### each/2 ###


<pre><code>
each(IteratorRef, EachFun) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>EachFun = fun((Data) -&gt; any())</code></li><li><code>IteratorRef = <a href="#type-ref">ref()</a></code></li><li><code>Data = term()</code></li><li><code>Reason = term()</code></li></ul>

Iterate over histogram applying a function to each data point
<a name="each-3"></a>

### each/3 ###


<pre><code>
each(IteratorRef, EachFun, Initial) -&gt; {ok, Accum} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>EachFun = fun(({IteratorType, Data}, Acc) -&gt; any())</code></li><li><code>IteratorType = record | linear | logarithmic | percentile</code></li><li><code>Data = [{atom(), term()}]</code></li><li><code>Acc = term()</code></li><li><code>IteratorRef = <a href="#type-ref">ref()</a></code></li><li><code>Reason = term()</code></li><li><code>Initial = term()</code></li><li><code>Accum = term()</code></li></ul>

Iterate over histogram applying a function to each data point accumulating a result
<a name="open-3"></a>

### open/3 ###


<pre><code>
open(IterationType, HdrRef, Options) -&gt; {ok, Ref} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>IterationType = record | linear | logarithmic | percentile</code></li><li><code>HdrRef = <a href="#type-ref">ref()</a></code></li><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Options = [<a href="proplists.md#type-property">proplists:property()</a>]</code></li><li><code>Reason = term()</code></li></ul>

Open a fresh instance of a high dynamic range (HDR) histogram iterator
