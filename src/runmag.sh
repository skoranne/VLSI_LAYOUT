#!/bin/bash
export OMP_PROC_BIND=TRUE
export OMP_PLACES=cores
export OMP_DISPLAY_ENV=true
INSTALL_PATH=/home/skoranne/GITHUB/VLSI_LAYOUT/
BINARY=MAGPARSER.exe
echo "Running command: $@"
$INSTALL_PATH/bin/$BINARY $@
