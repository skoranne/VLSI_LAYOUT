# NURBS GPU Kernel (scaffold)

Modern idiomatic Fortran + CUDA C++ (`iso_c_binding`) path for degree <= 3 rational NURBS/conic curvilinear geometry.

## Files
- `nurbs_curve.f90` - Fortran module: `BezSpan` (bind(c)), de Casteljau eval/split, y-extrema, y-monotone split, Green area/perim, curve1/mems1 helpers.
- `test_nurbs.f90` - regression: circle area, curve1 area/perim vs tessellation ref, y-monotone split, split consistency.
- `nurbs_gpu/nurbs_types.hpp`, `nurbs_bezier.hpp`, `nurbs_bool_cpu.hpp`, `nurbs_gpu.cu`, `test_nurbs_cpu.cpp` - CUDA C++ scaffold (N1 split, N2 AABB, pool API placeholders).
- `db/regressions/nurbs/` - regression script + curve1/mems1 Python reference + Clipper2 bench.

## Build/Run
```sh
export PATH="/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/bin:$PATH"
make -f Makefile.nvf test_nurbs.exe -j4
./test_nurbs.exe
bash db/regressions/nurbs/run_nurbs_regression.sh
```

## Curve1 baseline (NURBS-BRep-2D-v1, tessellation reference)
- nseg=4096: area=4305.779560338 perim=401.332712656
- Clipper2 (nseg=512, NonZero): area=4305.730346680 union_ms=0.715 offset_d=1.5 area=4901.543150000 offset_ms=3.253

## Status
Implemented: degree<=3 rational Bezier eval/split/y-monotone/area/perim; curve1+mems1 regression; CUDA scaffold.
Pending: exact rational y-extrema, N2 curve-curve intersection, N3 curved-slab Boolean sweep, N4 offsetting, N5 100B banded streaming, MAGPARSER iso_c_binding integration, CGAL/OCCT/CavalierContours/SISL adapters.
