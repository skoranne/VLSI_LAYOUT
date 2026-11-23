CXX ?= g++
AR ?= ar
CXXFLAGS ?= -O2 -std=c++17 -Wall -Wextra -I. -DLAYOUTDB_USE_ZLIB
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

CORE_SRCS = layoutdb.cpp layoutdb_hier.cpp gdsii.cpp oasis.cpp oasis_writer.cpp db_stats.cpp nvcomp_support.cpp lefdef_export.cpp prop_oasis_writer.cpp
CORE_OBJS = $(CORE_SRCS:.cpp=.o)

TESTS = MPW_MERGE db_extent db_extent_by_cell_layer db_hierarchy db_propnames db_roundtrip db_oasis2lefdef db_hier_extent bin2oas

all: $(TESTS) liblayoutdb.a

$(CORE_OBJS): %.o: %.cpp layoutdb.h layoutdb_internal.hpp db_stats.h fracture.hpp oasis_codec.hpp nvcomp_support.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

liblayoutdb.a: $(CORE_OBJS)
	$(AR) rcs $@ $^

bin2oas: bin2oas.cpp oasis_codec.hpp
	$(CXX) $(CXXFLAGS) -O3 $< -o $@ -lz

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
	rm -f $(CORE_OBJS) oasis_rec.o $(TESTS) *.rt *.oas.out

.PHONY: all clean
