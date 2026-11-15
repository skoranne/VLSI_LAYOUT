CXX ?= g++
CXXFLAGS ?= -O2 -std=c++17 -Wall -Wextra -I. -DLAYOUTDB_USE_ZLIB
LDFLAGS ?= -lz
NVCC ?= nvcc
# Optional: make NVCOMP=1 NVCOMP_INC=... NVCOMP_LIB=... to enable GPU CBLOCK decompression
ifdef NVCOMP
CXXFLAGS += -DLAYOUTDB_USE_NVCOMP
endif

CORE_SRCS = layoutdb.cpp gdsii.cpp oasis.cpp oasis_writer.cpp db_stats.cpp nvcomp_support.cpp
CORE_OBJS = $(CORE_SRCS:.cpp=.o)

TESTS = db_extent db_extent_by_cell_layer db_hierarchy db_propnames db_roundtrip

all: $(TESTS)

$(CORE_OBJS): %.o: %.cpp layoutdb.h layoutdb_internal.hpp db_stats.h
	$(CXX) $(CXXFLAGS) -c $< -o $@

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

clean:
	rm -f $(CORE_OBJS) $(TESTS) *.rt *.oas.out

.PHONY: all clean
