#!/usr/bin/env bash

BENCHMARKS_RUNNER=TRUE \
    BENCH_LIB=parallel_tutorial \
    dune exec --profile=release -- \
    ./bin/main.exe \
    -run-without-cross-library-inlining \
    -quota 1s
