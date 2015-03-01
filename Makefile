REBAR=$(shell [ -f ./rebar ] && echo "./rebar" || echo "rebar" )

.PHONY: all erl test clean doc 

all: test perf

build:
	$(REBAR) get-deps compile

buildplt:
	if [ ! -f .plt ]; then \
        dialyzer --build_plt --output_plt .plt --apps kernel stdlib ; \
    fi

pltclean:
	@rm .plt

dialyze:
	@ERL_LIBS=deps dialyzer --fullpath -Wno_undefined_callbacks \
        --plts .plt \
        -r ebin --src src \
        | grep -v -f ./dialyzer.ignore-warnings

test: build
	$(REBAR) skip_deps=true ct

eqc: build
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	-rm -f doc/*.md doc/edoc-info
	-rm -rvf deps ebin .eunit

perf: build
	clang -ggdb -c -O3 -ffast-math -std=c99 -I c_src perf/hh.c -o perf/hh.o
	clang -ggdb -lm -o perf/hh c_src/hdr_histogram.o perf/hh.o
	perf/hh 1000000 1000000 1
	perf/hh 100000000 1000000 1
	perf/hh 1000000000 1000000 1
	perf/hh 1000000 1000000 3
	perf/hh 100000000 1000000 3
	perf/hh 1000000000 1000000 3
	perf/hh 1000000 1000000 5
	perf/hh 100000000 1000000 5
	perf/hh 1000000000 1000000 5

doc: build
	$(REBAR) doc

demo-elixir: doc
	elixir -pa ebin -r examples/simple.exs -e "Simple.main"

demo-erlang: doc
	./examples/simple.erl

eqc-compile: build
	-mkdir ebin
	erl -make
