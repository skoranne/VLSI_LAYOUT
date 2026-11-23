#!/bin/bash
# OASIS + compact-layer studies, sequential (one guarded run at a time).
cd "$(dirname "$0")"
MAXL=40 ./run_study.sh ./bin_compact2 out_oas3 check_sdt16oas gpu MAGPARSER_COMPACT_LAYERS=1 MAGPARSER_COMPACT_BUDGET_GB=12 MAGPARSER_XOR_VIA_NOT=1 MAGPARSER_OR_VIA_NOT=1 MAGPARSER_PRESORT_GPU=1 MAGPARSER_GPU_MEM_BUDGET=4
F="MAGPARSER_COMPACT_LAYERS=1 MAGPARSER_COMPACT_BUDGET_GB=12 MAGPARSER_XOR_VIA_NOT=1 MAGPARSER_OR_VIA_NOT=1 MAGPARSER_PRESORT_GPU=1 MAGPARSER_GPU_MEM_BUDGET=4"
for c in sdt16oas fpu8x8oas mw64oas; do
  MAXL=40 ./run_study.sh ./bin_compact2 out_oas_study $c gpu $F
done
MAXL=40 ./run_study.sh ./bin_compact2 out_oas_study sdt16oas cpu $F
echo CHAIN DONE
