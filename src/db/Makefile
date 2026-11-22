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
# Optional: make NVCOMP=1 NVCOMP_INC=... NVCOMP_LIB=... to enable GPU CBLOCK decompression
ifdef NVCOMP
CXXFLAGS += -DLAYOUTDB_USE_NVCOMP
endif

CORE_SRCS = layoutdb.cpp layoutdb_hier.cpp gdsii.cpp oasis.cpp oasis_writer.cpp db_stats.cpp nvcomp_support.cpp lefdef_export.cpp
CORE_OBJS = $(CORE_SRCS:.cpp=.o)

TESTS = db_extent db_extent_by_cell_layer db_hierarchy db_propnames db_roundtrip db_oasis2lefdef db_hier_extent

all: $(TESTS) liblayoutdb.a

$(CORE_OBJS): %.o: %.cpp layoutdb.h layoutdb_internal.hpp db_stats.h fracture.hpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

liblayoutdb.a: $(CORE_OBJS)
	$(AR) rcs $@ $^

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

db_hier_extent: db_hier_extent.cpp $(CORE_OBJS)
	$(CXX) $(CXXFLAGS) $^ -o $@ $(LDFLAGS)

clean:
	rm -f $(CORE_OBJS) $(TESTS) *.rt *.oas.out

.PHONY: all clean
