#!/usr/bin/env bash

dune build --release && \
    PARALLEL_BENCH_DOMAINS=14 \
    BENCHMARKS_RUNNER=TRUE \
    BENCH_LIB=parallel_tutorial \
    ./_build/default/bin/main.exe -run-without-cross-library-inlining -quota 1s
