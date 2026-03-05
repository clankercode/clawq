# OCaml/Coq Binary Minimization Strategies

Collected strategies for reducing binary size of OCaml programs with Coq-extracted code.

---

## 1. OCaml Compiler Flags

### `-compact`
Optimizes generated native code for space rather than speed. Replaces inlined allocation sequences with function calls (`caml_alloc1`), producing smaller code that can paradoxically also be faster due to better branch prediction (Return Stack Buffer).

- Frama-C benchmark: 15.4 MB -> 14.4 MB (~7% reduction)
- Also showed slight speed improvement in some benchmarks
- **dune-workspace**: `(ocamlopt_flags (:standard -compact))`

Source: [Frama-C blog on -compact](https://www.frama-c.com/ocaml/2013/10/11/OCamls-option-compact-can-optimize-for-code-size-and-speed.html)

### `-nodynlink`
Enables static-linking optimizations by telling the compiler the binary won't be dynamically loaded. Produces tighter, non-relocatable code.

```
(ocamlopt_flags (:standard -nodynlink))
```

### Avoid `-linkall`
By default, OCaml only links referenced modules. Using `-linkall` forces all modules in linked libraries to be included. Ensure this flag is **not** set unless required (some libraries demand it).

### `-Oclassic` / Low `-inline`
Reduce inlining aggressiveness to limit code bloat:
- `-Oclassic`: reverts to traditional inlining, producing smaller `.cmx` files (**requires Flambda-enabled compiler switch**)
- `-inline 0`: prevents inlining except for trivially small functions (Flambda only)
- Lower `-inline` values (vs default 10, or -O2's 25, or -O3's 50) reduce code expansion

### `-without-runtime`
Excludes the OCaml runtime from the binary. Useful if managing the runtime separately.

### `-remove-unused-arguments` (Flambda)
Analyzes functions and removes unused parameters, reducing code size.

### Summary Table

| Flag | Effect | Size Impact |
|------|--------|-------------|
| `-compact` | Space-optimized code gen | ~7% smaller |
| `-nodynlink` | Static-link optimizations | Moderate |
| No `-linkall` | Only link referenced modules | Varies greatly |
| `-Oclassic` | Less aggressive inlining | Smaller code |
| `-inline 0..5` | Reduced inlining | Less code expansion |
| `-without-runtime` | Exclude runtime | Runtime-sized saving |
| `-remove-unused-arguments` | Drop unused params (Flambda) | Moderate |

Sources: [ocamlopt manual](https://ocaml.org/manual/5.4/native.html), [Flambda manual](https://ocaml.org/manual/5.4/flambda.html)

---

## 2. Flambda / Flambda2

### Flambda (available since OCaml 4.03)
Requires building OCaml with `-flambda` configure flag. Provides cross-unit optimization but **not** whole-program dead code elimination.

Within a compilation unit:
- Functors inlined, constants lifted, unused code eliminated
- Unused closure variables detected and removed
- Better inlining decisions with cost/benefit analysis

### Flambda2 (successor, developed by Jane Street/OCamlPro)
- Single-pass CPS traversal with downward propagation + upward dead code elimination
- More effective than Flambda1 at removing unnecessary expressions
- Jane Street is migrating all systems to Flambda2

### Whole-Program Dead Code Elimination (unmerged PR #608)
An experimental `-lto` flag for ocamlopt+Flambda that concatenates all Flambda IR at link time and runs whole-program dead code elimination.

**Results:**
- Mirage: 3.8 MB -> 1.3 MB (~66% reduction)
- Large executable: 220 MB -> 105 MB (~52% reduction)
- Another test: 23 MB -> 11 MB (~52% reduction)

**Caveats:**
- Never merged into mainline OCaml
- Compilation time increased significantly (1s -> 8s in one test, >10min in another)
- Prevents use of `dynlink`

Source: [PR #608](https://github.com/ocaml/ocaml/pull/608)

### The Tree-Shaking Gap
OCaml does **not** tree-shake across compilation units in mainline. The linker includes entire object files. This is the single biggest source of unnecessary binary bloat.

Source: [Dead code elimination dead end](https://www.chrisarmstrong.dev/posts/dead-code-elimination-dead-end-in-ocaml)

---

## 3. Coq Extraction Optimization

### Keep `Extraction Optimize` Enabled (default: on)
Controls type-preserving optimizations: constant inlining, beta/iota redex reduction, `Cases` simplifications. Disabling this produces larger, slower extracted code.

### `Extraction Inline` / `Extraction NoInline`
Control which definitions are inlined during extraction:
```coq
Extraction Inline sort_rec is_heap_rec.
Extraction NoInline list_to_heap.
```

### `Extract Inductive` - Map to Native Types
Replace Coq inductive types with OCaml built-ins to avoid bloated data representations:
```coq
Extract Inductive bool => "bool" [ "true" "false" ].
Extract Inductive list => "list" [ "[]" "(::)" ].
Extract Inductive nat => "int" [ "0" "(fun x -> x + 1)" ]
  "(fun zero succ n -> if n=0 then zero () else succ (n-1))".
```

**Critical:** Coq's unary `nat` extracts to deeply nested structures (7 = seven pointers deep). Mapping to `int` provides huge size and performance improvements.

### `Extract Inlined Constant` - Inline Primitives
Map Coq constants directly to OCaml expressions, eliminating wrapper functions:
```coq
Extract Inlined Constant plus => "( + )".
Extract Inlined Constant mult => "( * )".
Extract Inlined Constant eqb => "( = )".
Extract Inlined Constant ltb => "( < )".
```

### `Extract Constant` - Map Complex Operations
```coq
Extract Constant plus => "( + )".
```

### Use Standard Extraction Libraries
Import pre-configured mappings:
```coq
From Stdlib Require Import ExtrOcamlBasic.
From Stdlib Require Import ExtrOcamlString.
```

### Replace Coq Data Structures with OCaml Built-ins
Real-world example from VOQC project:
- Coq `nat` -> OCaml `int`
- Coq `Q` -> OCaml Zarith `Q`
- Coq `FMapAVL`/`FSetAVL` -> OCaml `Map`/`Set`

### Extract Only What You Need
Use targeted extraction commands rather than extracting entire libraries:
```coq
Extraction "file.ml" func1 func2.
```
Avoid pulling in proof terms, which significantly inflate extracted code.

### Use `Extraction Blacklist` for Name Conflicts
Prevents duplicate module names in extracted code.

### `Extraction Implicit` - Remove Proof/Type Arguments
Declare arguments that are useless in extracted code (proof terms, type parameters):
```coq
Extraction Implicit my_function [proof_arg type_arg].
(* Or by position (1-indexed): *)
Extraction Implicit my_function [3 5].
```

### `Unset Extraction Conservative Types`
Controls non-type-preserving optimizations. When unset (default), dummy type abstractions are removed more aggressively, producing smaller code.

### Singleton Type Optimization
When extraction produces a singleton type (one constructor, one argument), the inductive structure is automatically removed and treated as an alias. Disable with `Set Extraction KeepSingleton` if needed.

### `Separate Extraction` for Modularity
```coq
Separate Extraction func1 func2 func3.
```
Produces one `.ml` file per Coq `.v` source file instead of a monolithic file. Combined with linker GC (`--gc-sections`), this can help eliminate entire unused modules.

### Trim the `.mli` Interface
If the extracted `.mli` exposes functions you don't call from OCaml, those functions and their transitive dependencies cannot be eliminated by the compiler. Trim the `.mli` to expose only functions actually called from your runtime code.

### clawq-Specific Findings

The extracted `src/extracted/clawq_core.ml` (~1412 lines, ~86 KB) has several opportunities:

1. **`app` function**: Coq `List.app` is extracted as a recursive OCaml function instead of using `List.append`. Fix: `Extract Inlined Constant app => "List.append".` or ensure `Extract Inductive list` is set.

2. **`uint`, `uint0`, `uint1` types**: Decimal/hexadecimal numeral representation types from Coq's `Number` module, pulled in transitively by `Nat.of_num_uint`/`Nat.to_num_uint`. Almost certainly unused at runtime. Fix: `Extraction Inline` the functions that reference them.

3. **`eqb`, `leb`**: Use verbose encoding from `ExtrOcamlNatInt` when they could be native. Fix: `Extract Inlined Constant Nat.eqb => "(=)".` and `Extract Inlined Constant Nat.leb => "(<=)".`

4. **`tail_add`, `tail_addmul`, `tail_mul`**: Coq's tail-recursive nat arithmetic, unnecessary when nat is already mapped to int. Fix: inline to native OCaml arithmetic.

5. **`of_uint_acc` and numeral conversions**: Only needed if parsing/printing Coq-style numerals at runtime. Use `Extraction Inline` on these.

Sources: [Coq extraction manual](https://rocq-prover.org/doc/V8.18.0/refman/addendum/extraction.html), [Software Foundations - Extract](https://softwarefoundations.cis.upenn.edu/vfa-current/Extract.html), [VOQC project](https://github.com/inQWIRE/mlvoqc), [Verified Extraction from Coq to OCaml (ACM 2024)](https://dl.acm.org/doi/10.1145/3656379)

---

## 4. Stripping and Post-Link Processing

### `strip`
Remove debug symbols and symbol tables from the final binary:
```bash
strip your_binary
```
Community benchmarks for OCaml binaries:
- Simple OCaml program: 201 KB -> 113 KB (~44%)
- OCaml + Core library: 14 MB -> 8.1 MB (~42%)

**Never** use `strip` on bytecode executables from `ocamlc -custom` -- these are hybrid ELF+bytecode files and stripping breaks them. For `ocamlopt` native binaries, it is always safe.

### Separate Debug Symbols
```bash
objcopy --only-keep-debug your_binary your_binary.debug
strip your_binary
objcopy --add-gnu-debuglink=your_binary.debug your_binary
```

### Avoid `-g` in Release Builds
Dune includes debug symbols by default. For release builds, ensure `-g` is not passed:
```
(env (release (ocamlopt_flags (:standard -compact -nodynlink))))
```

### Linker-Level Optimizations
Pass through to the C linker via `-ccopt`:
```
-ccopt -Wl,--gc-sections       # Remove unused sections
-ccopt -Wl,--icf=all           # Identical Code Folding (~6% reduction)
-ccopt -ffunction-sections      # Per-function sections for gc-sections
-ccopt -fdata-sections          # Per-data sections for gc-sections
```

**Caveat on `--gc-sections`**: Limited effectiveness with OCaml (~0.14% in one test) because OCaml's frametable and module block maintain references to functions, preventing their removal. ICF (`--icf=all`) is more reliably useful.

Sources: [Stripping binaries discussion](https://discuss.ocaml.org/t/stripping-binaries/2308), [Large binaries analysis](https://discuss.ocaml.org/t/large-binaries-break-down-the-size-by-library/1098)

---

## 5. Static Linking with musl

### Why musl?
- glibc doesn't fully support static linking
- musl is lightweight and designed for static linking
- Statically linked musl binaries run on any Linux

### Using opam switches
```bash
opam switch create . --packages "ocaml-option-static,ocaml-option-musl,ocaml-option-no-compression,ocaml.5.1.1"
```
Requires `musl` and `musl-dev` packages on your system.

### Using Alpine Linux / Docker
Alpine uses musl natively, making static linking straightforward:
```dockerfile
FROM ocaml/opam:alpine
RUN opam install . --deps-only
RUN dune build --profile release
```

### Dune Configuration
```
(executable
 (name main)
 (link_flags (-cclib -static)))
```

Or via dune-workspace:
```
(env (static (link_flags (-cclib -static -ccopt -march=x86-64))))
```

### Caveats
- Need static `.a` versions of all C dependencies (e.g., `libsqlite3`)
- Debian makes static libs easier to obtain than Arch Linux
- Some packages require `g++` which `musl-tools` doesn't include

Sources: [OCamlPro static executables](https://ocamlpro.com/blog/2021_09_02_generating_static_and_portable_executables_with_ocaml/), [Tunbury.org static linking](https://www.tunbury.org/2025/06/17/static-linking/), [soap.coffee OCaml static binaries](https://soap.coffee/~lthms/posts/OCamlStaticBinaries.html)

---

## 6. UPX Compression

### Basic Usage
```bash
upx your_binary              # Default compression
upx --best your_binary       # Best compression ratio
upx --lzma your_binary       # LZMA algorithm (better for large binaries)
upx --brute your_binary      # Try all methods (slow)
upx --ultra-brute your_binary  # Try even harder (very slow)
```

Typically reduces binary size by 50-70%. OCaml-specific benchmarks:
- Simple OCaml (stripped): 113 KB -> 64 KB with UPX (68% from original)
- OCaml + Core (stripped): 8.1 MB -> 2.3 MB with UPX (84% from original)

### UPX + OCaml Compatibility Warning
Some reports indicate UPX can cause **core dumps** with OCaml native binaries. Test thoroughly before deploying UPX-compressed OCaml binaries.

### Important: musl + UPX Compatibility
- Dynamically linking against musl **causes segfaults** when compressed with UPX
- Statically linking against musl works fine with UPX
- Always use static musl builds if combining with UPX

### Decompression Overhead
UPX-compressed binaries decompress at startup. This adds a small startup latency but no ongoing runtime penalty.

Source: [UPX homepage](https://upx.github.io/), [UPX issue #93](https://github.com/upx/upx/issues/93)

---

## 7. Binary Size Analysis Tools

### `nm` - Symbol Analysis
```bash
nm -n your_binary | grep '_code_begin\|_code_end'
```
Reveals code sections for each module. Useful for identifying which modules contribute most to binary size.

### `objdump` / `readelf`
```bash
readelf -S your_binary    # Show section sizes
objdump -h your_binary    # Show section headers with sizes
size your_binary           # Quick text/data/bss summary
```

### `module-size` (OCaml-specific)
[hannesm/module-size](https://github.com/hannesm/module-size) uses `nm` to analyze `_code_begin`/`_code_end` markers and estimate per-module size contributions in a compiled OCaml binary.

### Bloaty McBloatface
General-purpose binary size profiler that can break down size by symbol, section, or compilation unit.

---

## 8. Bytecode vs Native

OCaml bytecode (`ocamlc`) produces smaller executables than native code (`ocamlopt`), but runs significantly slower (~10x). Bytecode requires the OCaml runtime interpreter.

For distribution where size matters more than speed, bytecode with `ocamlc -custom` embeds the interpreter into the binary, but the result is not strippable.

---

## 9. Recommended Pipeline for clawq

Based on the existing `dune-workspace` profiles and project structure:

### Current Setup
- `release-speed`: `-O3`
- `release-size`: `-O2 -compact`

### Proposed Enhanced Pipeline

1. **Extraction phase**: Ensure all `Extract Inductive` / `Extract Inlined Constant` mappings are used in `coq/` for native OCaml types
2. **Compile**: `dune build --profile release-size` (already uses `-O2 -compact`)
3. **Add flags** to `release-size` profile:
   ```
   (release-size
     (ocamlopt_flags (:standard -O2 -compact -nodynlink))
     (link_flags (-ccopt -Wl,--gc-sections -ccopt -Wl,--icf=all)))
   ```
4. **Strip**: `strip _build_opt_size/default/src/main.exe`
5. **Optional UPX**: `upx --best _build_opt_size/default/src/main.exe` (if static-linked)

### For Maximum Reduction (future)
- Build with Flambda-enabled OCaml switch
- Use musl static linking via `ocaml-option-static` + `ocaml-option-musl`
- Apply whole-program DCE if/when PR #608 lands or a Flambda2 equivalent appears
- Compress with UPX

---

## 10. Quick Reference

| Strategy | Effort | Typical Reduction |
|----------|--------|-------------------|
| `strip` | Trivial | 40-44% |
| `-compact` | Trivial | ~7% |
| `-nodynlink` | Trivial | Small |
| Avoid `-linkall` | Check deps | Varies |
| Coq `Extract Inductive` to native types | Moderate | Reduces extracted code significantly |
| Coq `Extract Inlined Constant` | Moderate | Eliminates wrapper functions |
| Linker `--gc-sections` + `--icf=all` | Low | ~6% |
| UPX compression | Low (test carefully!) | 50-70% on top of other reductions |
| Flambda + low inline | Moderate (switch) | 20-30% |
| musl static linking | High (new switch) | Enables UPX, smaller libc |
| Whole-program DCE (Flambda LTO) | High (unmerged) | 50-66% |
