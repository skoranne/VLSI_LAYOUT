#!/bin/bash
echo "Converting $1 to $2 using KLayout"
INSTALL_PATH=/home/skoranne/GITHUB/VLSI_LAYOUT/src/
klayout -b -rd input=$1 -rd output=$2 -r $INSTALL_PATH/kconvert.rb
