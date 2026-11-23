#!/bin/bash
export OMP_PROC_BIND=TRUE
# NVHPC device memory manager caches every freed device block; on unified-memory GB10 that
# grows by GBs per op (measured 30 GB on SDT16). Off by default; override by exporting it.
export NVCOMPILER_ACC_MEM_MANAGE=${NVCOMPILER_ACC_MEM_MANAGE:-0}
export OMP_PLACES=cores
export OMP_DISPLAY_ENV=true
INSTALL_PATH=/home/skoranne/GITHUB/VLSI_LAYOUT/
BINARY=MAGPARSER.exe
echo "Running command: $@"
$INSTALL_PATH/bin/$BINARY $@
