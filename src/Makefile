################################################################################
# File   : Makefile
# Author : Sandeep Koranne
#
################################################################################
FORTRAN=ifx -O3
all: MAGPARSER.exe
clean:
	rm MAGPARSER.exe hash_mod.mod hashtable.o magparser.o mainmagparser.o
hashtable.o:	hashtable.f90
	$(FORTRAN) -c hashtable.f90
geometry.o:	geometry.f90
	$(FORTRAN) -c geometry.f90
magparser.o:	magparser.f90 geometry.f90
	$(FORTRAN) -c magparser.f90
mainmagparser.o:	mainmagparser.f90
	$(FORTRAN) -c mainmagparser.f90
MAGPARSER.exe:	hashtable.o geometry.o magparser.o mainmagparser.o 
	$(FORTRAN) mainmagparser.o magparser.o hashtable.o geometry.o -o MAGPARSER.exe -lz

