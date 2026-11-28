# Curvilinear Kernel Comparison (curve1.json, mems1.json, MEMS testcases)

## Environment
- Host: NVIDIA GB10 (CUDA 13.0, `nvcc`, `nvfortran` 26.1), sandbox `/scratch1/skoranne/CURVILINEAR_KERNELS`
- Kernels built & verified:
  - **CGAL 5.6** (header-only + `boost/multiprecision/gmp.hpp` + `-lgmp`, `Exact_predicates_exact_constructions_kernel` + `approximated_offset_2` exact arc-bounded Minkowski sum)
  - **Clipper2** (`libClipper2.a`)
  - **CavalierContours** (header-only `cavc`, patched `staticspatialindex.hpp` for `<cstdint>`/GCC)
  - **SISL** (`libsisl.a`)
  - **MAGPARSER Native Curvilinear NURBS** (`NurbsCurveModule` + `NurbsBoolModule` in Modern Fortran + CUDA `nurbs_gpu.cu` N1/N2 on GB10)

## `curve1.json` (`NURBS-BRep-2D-v1`, 5 outer + 2 hole deg 1/2 rational)

| Kernel | Operation | Area ($\mu\text{m}^2$) | Notes |
|---|---|---:|---|
| `NurbsCurveModule` (16-pt Gauss-Legendre) | exact face | `4305.7795645` | zero tessellation, $10^{-14}$ quadrature |
| `NurbsBoolModule` (GB10 GPU N1+N2+N3) | `NURBS_NONZERO` slabs | `4305.7795645` | exact `CurvedTrap` decomposition |
| `nurbs_eval.py` (`nseg=4096`) | tessellation ref | `4305.7795603` | `perim=401.3327127` |
| Clipper2 (`nseg=512`, `NonZero`) | union | `4305.7303467` | `0.715 ms` |
| **CGAL `approximated_offset_2`** | outward offset $d=1.5$ | **`4901.2406`** | exact circular-arc Minkowski sum (`114.6 ms`, `holes=1`) |
| **Clipper2 `InflatePaths(Round)`** | outward offset $d=1.5$ | **`4901.5432`** | `3.253 ms`, `holes=1` |
| **MAGPARSER `NurbsBoolModule::nurbs_offset`** | outward offset $d=1.5$ | **`4901.3751`** | **native curvilinear NURBS, GB10 GPU, `0.0027%` vs CGAL** |
| CavalierContours `parallelOffset` | offset $d=1.0$ / OU | `3851.7621` / `4300.6275` | `5.46 ms` / `7.39 ms` |

## Native Curvilinear N1–N5 Verification (`test_nurbs.exe`, GB10 `gpu=T`)
- Unit circle GL area/perim: $\pi$ / $2\pi$ to $< 10^{-12}$.
- Böhm knot insertion (`nurbs_to_beziers`) on `mems1.json` 9-pt hub circle ($R=38$): $\pi R^2$ to $< 10^{-9}$.
- Overlapping unit circles (`OR`, `AND`, `NOT`, `XOR`): exact analytical lens formula to $< 10^{-7}$; GPU vs CPU parity $< 10^{-11}$.
- Native curvilinear offset (`nurbs_offset`): circle $+0.5$ ($\pi \cdot 1.5^2$) & $-0.3$ ($\pi \cdot 0.7^2$) to $< 10^{-5}$; two-circle comb-gap collision ($2d > g$) global self-intersection trim to $< 10^{-4}$.
- N5 Y-band streaming (`nurbs_banded_boolean`, $M=1,4,16$): exact area equivalence to $< 10^{-6}$.

## MEMS 1dbu OVER_UNDER / UNDER_OVER (`run_over_under.sh`)
See `OVER_UNDER_REPORT.md`: `COMPARE PASS` on all 6 MEMS testcases (orig parity `~0.0001%`, OU/UO delta `0.002%–0.350%`).
