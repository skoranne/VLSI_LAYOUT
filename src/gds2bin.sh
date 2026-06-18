#!/bin/bash
echo "Converting $1 to $2 using TOP_CELL_NAME=$3"
INSTALL_PATH=/home/skoranne/GITHUB/VLSI_LAYOUT/src/
klayout -b -r $INSTALL_PATH/gds2bin.rb -rd input_file=$1 -rd output_file=$2
