#!/bin/bash
cd "$(dirname "$0")"
export MAX_USE_GB=55 MIN_AVAIL_GB=20
./run_study.sh bin_inst/MAGPARSER.exe out_before sdt16 gpu
./run_study.sh bin_new/MAGPARSER.exe  out_after  sdt16 gpu
./run_study.sh bin_new/MAGPARSER.exe  out_after  sdt16 cpu
./run_study.sh bin_new/MAGPARSER.exe  out_after_xv sdt16 gpu MAGPARSER_XOR_VIA_NOT=1
echo CHAIN_DONE > chain_sdt16.done
