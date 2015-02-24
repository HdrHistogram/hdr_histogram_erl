

# Module hdr_histogram #
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add-2">add/2</a></td><td>Contribute the data points from a HDR histogram to another.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>Close this HDR histogram instance and free any system resources.</td></tr><tr><td valign="top"><a href="#count_at-2">count_at/2</a></td><td>Get the count of values at a given at a given value.</td></tr><tr><td valign="top"><a href="#from_binary-1">from_binary/1</a></td><td>Take a snapshot of HDR histogram internal state as a compressed binary and hydrate/open a reference.</td></tr><tr><td valign="top"><a href="#get_memory_size-1">get_memory_size/1</a></td><td>Get memory footprint of an HDR histogram.</td></tr><tr><td valign="top"><a href="#get_total_count-1">get_total_count/1</a></td><td>Get total count of record values.</td></tr><tr><td valign="top"><a href="#log-3">log/3</a></td><td>Log the histogram to a file in classic or CSV format.</td></tr><tr><td valign="top"><a href="#lowest_at-2">lowest_at/2</a></td><td>Get the lowest equivalent value.</td></tr><tr><td valign="top"><a href="#max-1">max/1</a></td><td>Get the maximum recorded data point value.</td></tr><tr><td valign="top"><a href="#mean-1">mean/1</a></td><td>Get the mean data point value to a significant figure.</td></tr><tr><td valign="top"><a href="#median-1">median/1</a></td><td>Get the median data point value to a significant figure.</td></tr><tr><td valign="top"><a href="#min-1">min/1</a></td><td>Get the minimum recorded data point value.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Open a fresh instance of a high dynamic range (HDR) histogram.</td></tr><tr><td valign="top"><a href="#percentile-2">percentile/2</a></td><td>Get the specified percentile  data point value to a significant figure.</td></tr><tr><td valign="top"><a href="#print-2">print/2</a></td><td>Print the histogram to standard output in classic or CSV format.</td></tr><tr><td valign="top"><a href="#record-2">record/2</a></td><td>Record an uncorrected histogram data point value in a HDR histogram.</td></tr><tr><td valign="top"><a href="#record_corrected-3">record_corrected/3</a></td><td>Record a histogram data point value in a HDR histogram with
expected interval for correction for coordinated ommission.</td></tr><tr><td valign="top"><a href="#record_many-3">record_many/3</a></td><td>Record a histogram data point value and number of occurances.</td></tr><tr><td valign="top"><a href="#reset-1">reset/1</a></td><td>Reset the memory backing this HDR histogram instance and zero results.</td></tr><tr><td valign="top"><a href="#same-3">same/3</a></td><td>Are two data point values considered to be equivalent.</td></tr><tr><td valign="top"><a href="#stddev-1">stddev/1</a></td><td>Get the standard deviation data point value to a significant figure.</td></tr><tr><td valign="top"><a href="#to_binary-1">to_binary/1</a></td><td>Take a snapshot of HDR histogram internal state as a compressed binary.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add-2"></a>

### add/2 ###


<pre><code>
add(To, From) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>To = <a href="#type-ref">ref()</a></code></li><li><code>From = <a href="#type-ref">ref()</a></code></li><li><code>Reason = term()</code></li></ul>

Contribute the data points from a HDR histogram to another
<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Ref) -&gt; ok | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li></ul>

Close this HDR histogram instance and free any system resources
<a name="count_at-2"></a>

### count_at/2 ###


<pre><code>
count_at(Ref, Value) -&gt; CountAt
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Value = integer()</code></li><li><code>CountAt = integer()</code></li></ul>

Get the count of values at a given at a given value
<a name="from_binary-1"></a>

### from_binary/1 ###


<pre><code>
from_binary(Binary) -&gt; {ok, Ref} | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Binary = binary()</code></li><li><code>Ref = <a href="#type-ref">ref()</a></code></li></ul>

Take a snapshot of HDR histogram internal state as a compressed binary and hydrate/open a reference. The reference SHOULD be closed when no longer needed to reclaim the memory used
<a name="get_memory_size-1"></a>

### get_memory_size/1 ###


<pre><code>
get_memory_size(Ref) -&gt; Size
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Size = integer()</code></li></ul>

Get memory footprint of an HDR histogram
<a name="get_total_count-1"></a>

### get_total_count/1 ###


<pre><code>
get_total_count(Ref) -&gt; Count
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Count = integer()</code></li></ul>

Get total count of record values
<a name="log-3"></a>

### log/3 ###


<pre><code>
log(Ref, Format, FileName::<a href="file.md#type-name">file:name()</a>) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Format = classic | csv</code></li><li><code>Reason = term()</code></li></ul>

Log the histogram to a file in classic or CSV format
<a name="lowest_at-2"></a>

### lowest_at/2 ###


<pre><code>
lowest_at(Ref, Value) -&gt; LowestValueAt
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Value = integer()</code></li><li><code>LowestValueAt = float()</code></li></ul>

Get the lowest equivalent value
<a name="max-1"></a>

### max/1 ###


<pre><code>
max(Ref) -&gt; MaxValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>MaxValue = integer()</code></li></ul>

Get the maximum recorded data point value
<a name="mean-1"></a>

### mean/1 ###


<pre><code>
mean(Ref) -&gt; MeanValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>MeanValue = float()</code></li></ul>

Get the mean data point value to a significant figure
<a name="median-1"></a>

### median/1 ###


<pre><code>
median(Ref) -&gt; MedianValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>MedianValue = float()</code></li></ul>

Get the median data point value to a significant figure
<a name="min-1"></a>

### min/1 ###


<pre><code>
min(Ref) -&gt; MinValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>MinValue = integer()</code></li></ul>

Get the minimum recorded data point value
<a name="open-2"></a>

### open/2 ###


<pre><code>
open(HighestTrackableValue, SignificantFigures) -&gt; {ok, Ref} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>HighestTrackableValue = integer()</code></li><li><code>SignificantFigures = 1..5</code></li><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Reason = term()</code></li></ul>

Open a fresh instance of a high dynamic range (HDR) histogram
<a name="percentile-2"></a>

### percentile/2 ###


<pre><code>
percentile(Ref, Percentile) -&gt; PercentileValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Percentile = float()</code></li><li><code>PercentileValue = float()</code></li></ul>

Get the specified percentile  data point value to a significant figure
<a name="print-2"></a>

### print/2 ###


<pre><code>
print(Ref, Format) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Format = classic | csv</code></li><li><code>Reason = term()</code></li></ul>

Print the histogram to standard output in classic or CSV format
<a name="record-2"></a>

### record/2 ###


<pre><code>
record(Ref, Value) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Value = integer()</code></li><li><code>Reason = term()</code></li></ul>

Record an uncorrected histogram data point value in a HDR histogram
<a name="record_corrected-3"></a>

### record_corrected/3 ###


<pre><code>
record_corrected(Ref, Value, ExpectedInterval) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Value = integer()</code></li><li><code>ExpectedInterval = integer()</code></li><li><code>Reason = term()</code></li></ul>

Record a histogram data point value in a HDR histogram with
expected interval for correction for coordinated ommission
<a name="record_many-3"></a>

### record_many/3 ###


<pre><code>
record_many(Ref, Value, Count) -&gt; ok | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>Value = integer()</code></li><li><code>Count = integer()</code></li><li><code>Reason = term()</code></li></ul>

Record a histogram data point value and number of occurances
<a name="reset-1"></a>

### reset/1 ###


<pre><code>
reset(Ref) -&gt; ok | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li></ul>

Reset the memory backing this HDR histogram instance and zero results
<a name="same-3"></a>

### same/3 ###


<pre><code>
same(Ref, A, B) -&gt; AreEquivalent
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>A = integer()</code></li><li><code>B = integer()</code></li><li><code>AreEquivalent = boolean()</code></li></ul>

Are two data point values considered to be equivalent
<a name="stddev-1"></a>

### stddev/1 ###


<pre><code>
stddev(Ref) -&gt; StddevValue
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li><li><code>StddevValue = float()</code></li></ul>

Get the standard deviation data point value to a significant figure
<a name="to_binary-1"></a>

### to_binary/1 ###


<pre><code>
to_binary(Ref) -&gt; binary() | {error, term()}
</code></pre>

<ul class="definitions"><li><code>Ref = <a href="#type-ref">ref()</a></code></li></ul>

Take a snapshot of HDR histogram internal state as a compressed binary. The reference HDR instance can be modified after a snapshot is taken in the usual way with no restrictions.
