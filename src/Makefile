################################################################################
# File   : Makefile
# Author : Sandeep Koranne
#
################################################################################
HDF5_FC=ifx
#FORTRAN=/scratch1/skoranne/INTELCOMPILERS/bin/h5fc -check bounds -check all -traceback -O3 -parallel -qopenmp -qopt-report=3
FORTRAN=/scratch1/skoranne/INTELCOMPILERS/bin/h5fc -O3 -parallel -qopenmp -qopt-report=3
#FORTRAN=ifx -O3 -parallel
LIBS=/scratch1/skoranne/INTELCOMPILERS/lib/
all: MAGPARSER.exe
clean:
	rm MAGPARSER.exe hash_mod.mod hashtable.o magparser.o mainmagparser.o rtree.o geometry.o
hashtable.o:	hashtable.f90
	$(FORTRAN) -c hashtable.f90
datastructures.o:	datastructures.f90
	$(FORTRAN) -c datastructures.f90
geometry.o:	geometry.f90
	$(FORTRAN) -c geometry.f90
design.o:	design.f90
	$(FORTRAN) -c design.f90
controlparser.o:	controlparser.f90
	$(FORTRAN) -c controlparser.f90
pnummerge.o:	pnummerge.f90
	$(FORTRAN) -c pnummerge.f90
hdfdata.o:	hdfdata.f90 geometry.f90
	$(FORTRAN) -c hdfdata.f90
rtree.o:	rtree.f90 geometry.f90
	$(FORTRAN) -c rtree.f90
contour.o:	contour.f90 geometry.f90
	$(FORTRAN) -c contour.f90
magparser.o:	magparser.f90 geometry.f90 rtree.f90
	$(FORTRAN) -c magparser.f90
mainmagparser.o:	mainmagparser.f90 controlparser.o
	$(FORTRAN) -c mainmagparser.f90
unit_test:	test_contour.exe
test_contour.o:	test_contour.f90
	$(FORTRAN) -c test_contour.f90
test_contour.exe:	test_contour.o contour.o datastructures.o
	$(FORTRAN) datastructures.o hdfdata.o design.o geometry.o  rtree.o pnummerge.o contour.o test_contour.f90 -o test_contour.exe -L$(LIBS) -lz -lhdf5_fortran -lhdf5
MAGPARSER.exe:	hashtable.o geometry.o design.o controlparser.o hdfdata.o datastructures.o rtree.o pnummerge.o magparser.o mainmagparser.o
	$(FORTRAN) mainmagparser.o magparser.o hashtable.o datastructures.o hdfdata.o design.o geometry.o  rtree.o pnummerge.o controlparser.o -o MAGPARSER.exe -L$(LIBS) -lz -lhdf5_fortran -lhdf5

