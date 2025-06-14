# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an OxCaml (Jane Street's branch of OCaml) parallel programming tutorial project that explores:
- Parallel algorithms on tree data structures
- OxCaml's mode system (portable, contended, etc.)
- OxCaml's kind system (mutable_data, immutable_data)
- Atomic operations and concurrent data structures
- Fork-join parallelism using the Parallel module

## Build Commands

```bash
# Build everything including tests and check formatting
dune build @default @runtest @fmt

# Fix formatting and promote expect test outputs
dune promote

# Run a single test file
dune runtest tree.ml

# Clean build artifacts
dune clean
```

## Dependencies

The project uses the following key libraries (as specified in dune):
- `core` - Jane Street's standard library replacement
- `async` - Jane Street's async library
- `parallel` - OCaml's parallel programming library
- `parallel.scheduler.work_stealing` - Work-stealing scheduler for parallel computations
- `ppx_jane` - Jane Street's PPX extensions

## Code Architecture

- `tree.ml` - Basic tree data structure definition
- `thing_tree.ml` - Example parallel algorithms on trees containing "Thing" objects with modes
- `import.ml` - Common imports and utilities
- Files exploring atomics and concurrent operations (like `thing_tree_atomic.ml`)

## Key Patterns

The codebase explores OxCaml's type system features:

**Modes** (properties of values):
- `@ portable` - Functions that can be used across parallel domains
- `@ contended` - Values that may be accessed concurrently

**Kinds** (properties of types):
- `: mutable_data` - Types that contain mutable fields
- `: immutable_data` - Types with only immutable fields

Example parallel pattern used throughout:
```ocaml
Parallel.fork_join2 par
  (fun par -> computation1 par arg1)
  (fun par -> computation2 par arg2)
```

## Known Issues (from notes.md)

- Mode inference can be tricky - explicit annotations often needed
- Error messages for mode violations can be confusing
- Merlin may show false syntax errors with mode annotations