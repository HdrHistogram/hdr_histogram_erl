## Investigate bug. Bug may be with nif or erlang

### make test

```
$ make test
rebar get-deps compile
==> edown (get-deps)
==> hdr_histogram_erl (get-deps)
==> edown (compile)
==> hdr_histogram_erl (compile)
  Checking whether the PLT .plt is up-to-date... yes
  Proceeding with analysis...
Unknown functions:
 done in 0m0.38s
done (passed successfully)
rebar skip_deps=true ct
==> hdr_histogram_erl (ct)
ERROR: sh(grep -e "TEST COMPLETE" -e "{error,make_failed}" /home/darach/Personal/Github/hdr_histogram_erl/logs/raw.log)
failed with return code 1 and the following output:

ERROR: ct failed while processing /home/darach/Personal/Github/hdr_histogram_erl: rebar_abort
Makefile:25: recipe for target 'test' failed
make: *** [test] Error 1
```

### rebar ct

```
$ rebar skip_deps=true ct
==> hdr_histogram_erl (ct)
DONE.
Testing Github.hdr_histogram_erl: TEST COMPLETE, 12 ok, 0 failed of 12 test cases
```

### make test with USE_GDB=1

```
$ USE_GDB=1 make test
rebar get-deps compile
==> edown (get-deps)
==> hdr_histogram_erl (get-deps)
==> edown (compile)
==> hdr_histogram_erl (compile)
  Checking whether the PLT .plt is up-to-date... yes
  Proceeding with analysis...
Unknown functions:
 done in 0m0.41s
done (passed successfully)
rebar skip_deps=true ct
==> hdr_histogram_erl (ct)
DONE.
Testing Github.hdr_histogram_erl: TEST COMPLETE, 12 ok, 0 failed of 12 test cases
```

### logs/raw.log

```
--- Test run on 2014/11/23 01:49:15 ---
Converting "/home/darach/Personal/Github/hdr_histogram_erl/." to "/home/darach/Personal/Github/hdr_histogram_erl" and re-inserting with add_patha/1
Converting "ebin" to "/home/darach/Personal/Github/hdr_histogram_erl/ebin" and re-inserting with add_patha/1
Converting "test" to "/home/darach/Personal/Github/hdr_histogram_erl/test" and re-inserting with add_patha/1


Common Test v1.8.2 starting (cwd is /home/darach/Personal/Github/hdr_histogram_erl)


Common Test: Running make in test directories...

CWD set to: "/home/darach/Personal/Github/hdr_histogram_erl/logs/ct_run.hdr_histogram_test@darach.local.2014-11-23_01.49.15"

TEST INFO: 1 test(s), 12 case(s) in 1 suite(s)

Testing Github.hdr_histogram_erl: Starting test, 12 test cases
*** Error in `/opt/kerl/r17x03ds/erts-6.2/bin/beam.smp': corrupted double-linked list: 0x00007f1d90000aa0 ***
```

Same issue / workarounds occur with R16b2 as R17 with dirty schedulars enabled
