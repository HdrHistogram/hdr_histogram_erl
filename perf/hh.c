// Copyright (c) 2014 Darach Ennis < darach at gmail dot com >.
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit
// persons to whom the Software is furnished to do so, subject to the
// following conditions:  
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
// NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
// USE OR OTHER DEALINGS IN THE SOFTWARE.

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <inttypes.h>
#include <errno.h>
#include <sys/time.h>

#include "hdr_histogram.h"

int main(int argc, char** argv)
{
    if (argc != 4 && atol(argv[1]) < 1L && atol(argv[2]) && atoi(argv[3]) < 1)
    {
        fprintf(stderr, "usage:\n\t%s <iterations> <high> <sig>\n\n", argv[0]);
        fprintf(stderr, "example:\n\t%s 10000000 1000000 3\n\n", argv[0]);
        return 1;
    }

    unsigned long n = atol(argv[1]);
    unsigned long h = atoi(argv[2]);
    unsigned int f = atoi(argv[3]);

    hdr_histogram_t* hh;
    hdr_alloc(h,f,&hh);

    struct timeval s, e;
    gettimeofday(&s,NULL);
    for (int i = 0; i < n; i++)
    {
        hdr_record_value(hh, i % h);
    }
    gettimeofday(&e,NULL);

    double es = (e.tv_sec - s.tv_sec) + ((double)(e.tv_usec - s.tv_usec)/1e6); 
    double en = es * 1e9;
    double hps = (es >= 1.) ? (double)n / es : (double)n * es;
    double nsh = en / n;
    printf("nh/perf: n: %lu h: %lu sf: %i elapsed: %f (%f hps, %f nsh).\n", n, h, f, es, hps, nsh);

    return 0;
}
