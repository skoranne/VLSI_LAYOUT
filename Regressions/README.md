# Regressions

Regression tests for MAGPARSER (NVIDIA/nvf + OpenMP target offload focus).

## Boolean AND/OR/NOT/XOR (nvf)

- `boolean_ops_nvf.ctr`: exercises Boolean operators (`*` AND, `+` OR, `^` XOR, `@`/`!` NOT) on `examples/INV.mag` with XOR identity checks.
- `run_boolean_regression.sh`: helper runner.

Run from `src/` (uses nvf-built `MAGPARSER.exe`):

```sh
cd src
export PATH=/opt/nvidia/hpc_sdk/Linux_aarch64/25.9/compilers/bin:$PATH  # nvhpc/25.9
./MAGPARSER.exe ../Regressions/boolean_ops_nvf.ctr 10
# or
./run_boolean_regression.sh ../Regressions/boolean_ops_nvf.ctr
```

