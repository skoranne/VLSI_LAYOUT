#!/bin/bash
cd "$(dirname "$0")"
export MAX_USE_GB=60 MIN_AVAIL_GB=18 MAGPARSER_GPU_MEM_BUDGET=8
FLAGS="MAGPARSER_XOR_VIA_NOT=1 MAGPARSER_OR_VIA_NOT=1 MAGPARSER_PRESORT_GPU=1"
./run_study.sh bin_new2/MAGPARSER.exe out_after_all fpu8x8g gpu $FLAGS
./run_study.sh bin_new2/MAGPARSER.exe out_after fpu8x8g cpu
./run_study.sh bin_inst/MAGPARSER.exe out_before fpu8x8g gpu
./run_study.sh bin_new2/MAGPARSER.exe out_after_all mw64 gpu $FLAGS
./run_study.sh bin_new2/MAGPARSER.exe out_after mw64 cpu
echo DONE > chain_big.done
