CXX ?= g++
AR ?= ar
CXXFLAGS ?= -O2 -std=c++17 -Wall -Wextra -I. -DLAYOUTDB_USE_ZLIB
# Position-independent so the same objects build liblayoutdb.so (Julia ccall shims).
CXXFLAGS += -fPIC
LDFLAGS ?= -lz
# The Fortran link on macOS (ifort) targets an older libc++ than the default
# SDK; without a matching deployment target std::function leaves undefined
# symbols (bad_function_call) in MAGPARSER.exe.
ifeq ($(shell uname -s),Darwin)
CXXFLAGS += -mmacosx-version-min=11.0
endif
NVCC ?= nvcc
# GPU CBLOCK decompression via nvCOMP: auto-detected (see nvcomp.mk; NVCOMP=0 disables).
include $(dir $(lastword $(MAKEFILE_LIST)))nvcomp.mk
CXXFLAGS += $(NVCOMP_CXXFLAGS)
LDFLAGS += $(NVCOMP_LDLIBS) $(NVCOMP_CUDART) -pthread

CORE_SRCS = layoutdb.cpp layoutdb_hier.cpp anyangle_capi.cpp trap_out.cpp gdsii.cpp oasis.cpp oasis_writer.cpp oasis_model_writer.cpp db_stats.cpp nvcomp_support.cpp lefdef_export.cpp oasis_tables.cpp oasis_sidecar.cpp prop_oasis_writer.cpp def_probe.cpp
CORE_OBJS = $(CORE_SRCS:.cpp=.o)

TESTS = MPW_MERGE db_extent db_extent_by_cell_layer db_hierarchy db_propnames db_roundtrip db_oasis2lefdef db_hier_extent bin2oas db_oasinfo db_oas_stats db_mvi db_defcheck db_def2oas db_oasis_selftest db_rotcheck

all: $(TESTS) liblayoutdb.a liblayoutdb.so

$(CORE_OBJS): %.o: %.cpp layoutdb.h layoutdb_internal.hpp db_stats.h fracture.hpp oasis_codec.hpp nvcomp_support.hpp oasis_parser.hpp oasis_writer.hpp flat_xform.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

liblayoutdb.a: $(CORE_OBJS)
	$(AR) rcs $@ $^

# Shared library for Julia (julia/LayoutDB) and other FFI users.
liblayoutdb.so: $(CORE_OBJS)
	$(CXX) -shared $^ -o $@ $(LDFLAGS)

db_mvi: db_mvi.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

# DEF/DEF.gz (+ LEF) vs layout cross-check (regressions/defcheck_mkALU.sh)
db_defcheck: db_defcheck.cpp def_reader.hpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $(filter %.cpp %.o %.a,$^) -o $@ $(LDFLAGS)

db_def2oas: db_def2oas.cpp def_reader.hpp oasis_codec.hpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $(filter %.cpp %.o %.a,$^) -o $@ $(LDFLAGS)

db_oasinfo: db_oasinfo.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

# Layer/bbox/text/property stats for OASIS/GDSII -> parseable text for CTR generation.
db_oas_stats: db_oas_stats.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

# OASIS/GDSII conformance self-test (spec tables, every record type, writer round trips);
# regressions/oasis_conformance.sh compares its outputs with KLayout.
db_oasis_selftest: db_oasis_selftest.cpp $(CORE_OBJS) oasis_rec.o
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

bin2oas: bin2oas.cpp oasis_writer.o oasis_writer.hpp oasis_codec.hpp
	$(CXX) $(CXXFLAGS) -O3 -fopenmp bin2oas.cpp oasis_writer.o -o $@ -lz

# Rotation check of the flatten logic (regress/rotation)
db_rotcheck: db_rotcheck.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_extent: db_extent.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_extent_by_cell_layer: db_extent_by_cell_layer.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_hierarchy: db_hierarchy.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_propnames: db_propnames.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_roundtrip: db_roundtrip.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

db_oasis2lefdef: db_oasis2lefdef.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

oasis_rec.o: oasis_rec.cpp oasis_rec.hpp oasis_codec.hpp fracture.hpp nvcomp_support.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# MPW shuttle assembly: per-slot OASIS files -> one OASIS with scribe lanes
MPW_MERGE: mpw_merge.cpp oasis_rec.o nvcomp_support.o oasis_rec.hpp oasis_codec.hpp
	$(CXX) $(CXXFLAGS) mpw_merge.cpp oasis_rec.o nvcomp_support.o -o $@ $(LDFLAGS) -pthread

db_hier_extent: db_hier_extent.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(CORE_OBJS) oasis_rec.o $(TESTS) liblayoutdb.a liblayoutdb.so *.rt *.oas.out

.PHONY: all clean
