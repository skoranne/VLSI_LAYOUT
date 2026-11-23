#!/bin/bash
# FPU8x8 and MW64 from OASIS with the compact store, current binary (one guarded run at a time).
cd "$(dirname "$0")"
B=${B:-./bin_compact4}; O=${O:-out_oas_v4}
F="MAGPARSER_COMPACT_LAYERS=1 MAGPARSER_COMPACT_BUDGET_GB=12 MAGPARSER_XOR_VIA_NOT=1 MAGPARSER_OR_VIA_NOT=1 MAGPARSER_PRESORT_GPU=1 MAGPARSER_GPU_MEM_BUDGET=4"
for c in ${CASES:-fpu8x8oas mw64oas}; do
  MAXL=40 ./run_study.sh $B $O $c gpu $F
done
echo CHAIN DONE
