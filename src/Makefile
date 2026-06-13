################################################################################
# File   : Makefile
# Author : Sandeep Koranne (Refactored)
################################################################################

# --- Compiler & Flags ---
FC = /scratch1/skoranne/INTELCOMPILERS/bin/h5fc
# Debug flags (commented out, but easily swappable)
# FCFLAGS = -check bounds -check all -traceback -O3 -parallel -qopenmp -qopt-report=3
FCFLAGS = -O3 -parallel -qopenmp -qopt-report=3
GIT_HASH := $(shell git rev-parse --short HEAD)
# --- Linker Variables ---
LDFLAGS = -L/scratch1/skoranne/INTELCOMPILERS/lib/
LDLIBS = -lz -lhdf5_fortran -lhdf5

# --- Library Definition ---
LIBRARY = libmagparser.a
LIB_OBJS = commonmodule.o systeminfo.o hashtable.o geometry.o design.o \
           controlparser.o hdfdata.o datastructures.o \
           rtree.o pnummerge.o magparser.o contour.o version_mod.o morton.o \
	   polygon_fracture.o boxmerge.o

# Create a list of all object files that will be compiled
ALL_OBJS = $(LIB_OBJS) mainmagparser.o test_contour.o

# Derive a list of dependency (.d) files from the object list
DEP_FILES = $(ALL_OBJS:.o=.d)

# --- Phony Targets ---
# Tells Make these aren't actual files to generate
.PHONY: all clean unit_test

all: MAGPARSER.exe
# This target writes a small .f90 module containing the hash
version_mod.f90: ../.git/HEAD ../.git/index
	@echo "module version_mod" > $@
	@echo "  implicit none" >> $@
	@echo "  character(len=*), parameter :: commit_hash = '$(GIT_HASH)'" >> $@
	@echo "end module version_mod" >> $@

unit_test: test_contour.exe test_morton.exe test_polygonfracture.exe test_boxmerge.exe \
	   test_skiplist.exe

clean:
	rm -f *.o *.mod *.a *.exe

# Generating specific versions
#merge_utils.o: merge_utils.F90 SkipList_template.inc
#    $(FC) $(FFLAGS) -DPAYLOAD_TYPE="type(ScanBox)" -DPROC_PREFIX="scanbox_" -c merge_utils.F90 -o merge_utils.o

# --- Pattern Rules ---
# This single rule replaces all the individual ".f90 -> .o" targets.
# $< = the first dependency (the .f90 file)
# $@ = the target (the .o file)
%.o: %.f90
	$(FC) $(FCFLAGS) -gen-dep=$(@:.o=.d) -c $< -o $@

# --- Library Creation ---
# Bundles all the core objects into a single static library.
# $^ = all dependencies (all files in LIB_OBJS)
$(LIBRARY): $(LIB_OBJS)
	ar rcs $@ $^

# --- Executable Builds ---
# By linking against the library, we don't have to list out all objects again.
MAGPARSER.exe: mainmagparser.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)

test_contour.exe: test_contour.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)
test_morton.exe: test_morton.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)
test_polygonfracture.exe: test_polygonfracture.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)
test_boxmerge.exe: test_boxmerge.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)
test_skiplist.exe: test_skiplist.o $(LIBRARY)
	$(FC) $(FCFLAGS) $^ -o $@ $(LDFLAGS) $(LDLIBS)

# --- Fortran Module Dependencies ---
# CRITICAL: If file B "USE"s a module from file A, file B.o depends on file A.o.
# This ensures Make compiles A.f90 first, generating the .mod file before B.f90 needs it.
hdfdata.o: geometry.o
rtree.o: geometry.o
contour.o: geometry.o
magparser.o: geometry.o rtree.o

# Main programs depend on the modules they USE directly
mainmagparser.o: controlparser.o systeminfo.o magparser.o hashtable.o datastructures.o hdfdata.o design.o rtree.o pnummerge.o geometry.o
test_contour.o: contour.o datastructures.o geometry.o hdfdata.o design.o rtree.o pnummerge.o
-include $(DEP_FILES)
