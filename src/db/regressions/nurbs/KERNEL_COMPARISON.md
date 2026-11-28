# Curvilinear Kernel Comparison (curve1.json, MEMS testcases)

## Environment
- Host: NVIDIA GB10, `/scratch1/skoranne/CURVILINEAR_KERNELS`
- Kernels built: Clipper2 (libClipper2.a), SISL (libsisl.a), CavalierContours (header-only, patched staticspatialindex.hpp for gcc), CGAL-5.6 (headers present; compile blocked by Boost.Multiprecision/gmp_rational vs CGAL_Core on this toolchain)
- OCCT 7.6 runtime installed; dev headers not present

## curve1.json (NURBS-BRep-2D-v1, 5 outer + 2 hole deg1/2 rational)

| Kernel | Metric | Result |
|---|---|---|
| nurbs_eval.py tess nseg=4096 | area | 4305.779560338 |
| nurbs_eval.py tess nseg=4096 | perim | 401.332712656 |
| Clipper2 nseg=512 NonZero union | area | 4305.730346680 |
| Clipper2 nseg=512 | union_ms | 0.715 |
| Clipper2 InflatePaths d=1.5 Round | area | 4901.543150000, ms=3.253 |
| CavalierContours parallelOffset d=1.0 (tess line poly) | area | 3851.762122999, ms=5.462 |
| CavalierContours OU (d=1,-1) | area | 4300.627542314, ms=7.387 |

## MEMS 1dbu OVER_UNDER/UNDER_OVER (d=1.0um, nm-int)

See `OVER_UNDER_REPORT.md`. MAGPARSER TrapMorph orig parity ~0.0001%; OU/UO delta 0.002%-0.350% vs Clipper2 on tessellated straight-edge inputs.

## Status
- Done: Clipper2 adapter+bench, CavC adapter+bench, SISL built, nurbs_curve.f90 (Modern Fortran), test_nurbs/test_nurbs_offset, 6 MEMS testcases, regressions.
- Blocked/env: CGAL compile (Boost MP/CGAL_Core compat), OCCT dev headers.
- Pending: native curvilinear NURBS offset (N4), GPU N1-N5 kernels, full MAGPARSER NURBS iso_c_binding integration, parametric P-cell generator expansion, 100B-shape streaming validation.
