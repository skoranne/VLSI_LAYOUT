# ENVIRONMENT.md: local toolchain notes

## Intel Fortran (ifx), used by `src/Makefile.ifx`

```bash
source /home/skoranne/LOAD_IFX.sh     # loads: tbb compiler-rt umf mpi compiler-intel-llvm/latest
which ifx                             # /scratch1/skoranne/intel/oneapi/compiler/2025.0/bin/ifx
ifx --version                         # ifx (IFX) 2025.0.0 20241008
```

- The modulefiles live in `~/modulefiles` (on `MODULEPATH` in a login shell); the oneAPI install is
  `/scratch1/skoranne/intel/oneapi` (`setvars.sh` there is an alternative way to load it).
- `compiler-intel-llvm/latest` requires `tbb` to be loaded first, so load the modules in the order the script uses.
- Source the script in the same shell that runs the build. `source ... | tail` runs it in a
  subshell and loses the environment. From a non-interactive tool:
  `bash -lc 'source ~/LOAD_IFX.sh && cd src && make -f Makefile.ifx all'`.
- ifx is **not** on PATH by default; `gfortran` (`/usr/bin`) and `h5fc` are.

## Other tools
- Julia: `~/.juliaup/bin/julia` (runs `trs_dag.jl`).
- Lean 4.36.0 via elan: `~/.elan/bin` (not on PATH by default); proofs in `lean/`.
