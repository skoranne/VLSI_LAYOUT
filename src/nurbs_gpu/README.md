# NURBS Curvilinear Engine (Modern Fortran + CUDA GPU)

Modern idiomatic Fortran (`iso_fortran_env`, `iso_c_binding`, `pure/elemental`, POD/SoA, no `class(...)`, `ifx`-safe) + CUDA 13.0 (`cudaMemPool_t`) kernel for degree $\le 3$ rational NURBS/conic geometry (**N1–N5**).

## Files
- `src/nurbs_curve.f90` (`NurbsCurveModule`): `bind(c)` POD `BezSpan` & `CurvedTrap`, homogeneous de Casteljau eval/split/subspan, exact analytical rational $C, C', C''$ & curvature $\kappa/\rho$, closed-form rational quadratic & analytical cubic `y_extrema`, exact `bez_x_at_y`, 16-pt Gauss-Legendre `bez_area2_gl`/`bez_perim_gl`, Böhm knot insertion (`nurbs_to_beziers`), recursive AABB + 2D Newton `bez_intersect`, and `iso_c_binding` GPU interfaces.
- `src/nurbs_bool.f90` (`NurbsBoolModule`):
  - **N1** GPU/CPU $y$-monotonic splitting (`prepare_monotone`)
  - **N2** GPU/CPU AABB overlap broadphase + exact curve-curve splitting (`split_intersections`)
  - **N3** Curved-slab Boolean sweep (`nurbs_curved_boolean`: `NONZERO`, `OR`, `AND`, `NOT`, `XOR`)
  - **N4** Native curvilinear offsetting with exact circular fillets & global self-intersection/cusp trim (`nurbs_offset`)
  - **N5** Bounded-memory $M$-band Y-streaming (`nurbs_banded_boolean`)
- `src/nurbs_gpu/nurbs_gpu.cu`: CUDA 13.0 N1 (`k_split_y_monotone`) & N2 (`k_aabb_bins`, `k_find_overlaps`) kernels backed by stream-ordered `cudaMemPool_t` (`cudaMallocAsync`/`cudaFreeAsync`), plus CPU fallback for `Makefile.ifx`.
- `src/test_nurbs.f90`, `src/test_nurbs_offset.f90`, `src/db/regressions/nurbs/`: unit tests & regressions (`run_nurbs_regression.sh`, `run_mems_testcases.sh`, `run_over_under.sh`).

## Build & Verify
```sh
export PATH="/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/bin:$PATH"
make -f Makefile.nvf test_nurbs.exe test_nurbs_offset.exe -j4
./test_nurbs.exe
bash db/regressions/nurbs/run_nurbs_regression.sh
bash db/regressions/nurbs/run_mems_testcases.sh
bash db/regressions/nurbs/run_over_under.sh
```
