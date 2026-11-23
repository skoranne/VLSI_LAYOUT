#!/bin/bash
# Usage: run_study.sh BIN OUTROOT CASE MODE [extra env K=V ...]
#   CASE: sdt16|fpu8x8|mw64 (uses study_CASE.ctr)   MODE: gpu|cpu|nsys
set -u
BIN=$(readlink -f "$1"); OUTROOT=$2; CASE=$3; MODE=$4; shift 4
HERE=$(cd "$(dirname "$0")" && pwd)
OUT=$OUTROOT/$CASE/$MODE; mkdir -p "$OUT"
export NVCOMPILER_TERM=trace OMP_PROC_BIND=TRUE OMP_PLACES=cores MAGPARSER_GPU_RESIDENCY_STATS=1 MAGPARSER_PHASE_TIMERS=1
for kv in "$@"; do export "$kv"; done
case $MODE in
  cpu) export OMP_TARGET_OFFLOAD=DISABLED ;;
  *)   export OMP_TARGET_OFFLOAD=MANDATORY ;;
esac
rm -rf "${CLAUDE_JOB_DIR:-/tmp}/tmp/study_$CASE"; mkdir -p "${CLAUDE_JOB_DIR:-/tmp}/tmp/study_$CASE"
cd "$OUT"
CMD=("$BIN" "$HERE/study_$CASE.ctr" ${MAXL:-10})
if [ "$MODE" = nsys ]; then
  CMD=(/usr/local/cuda/bin/nsys profile -t cuda,nvtx,openmp,osrt --cuda-memory-usage=true -f true -o "$OUT/prof" "${CMD[@]}")
fi
echo "START $(date -Is) $CASE $MODE $BIN" > run.meta
# Memory watchdog: on GB10 GPU allocations share host LPDDR5x but are NOT counted in RSS,
# so earlyoom/OOM-killer pick the wrong victims.  Sample MemAvailable every second
# (mem.trace) and SIGKILL the run if it drops below MIN_AVAIL_GB (default 16).
MIN_AVAIL_GB=${MIN_AVAIL_GB:-24}
MAX_USE_GB=${MAX_USE_GB:-50}       # kill if the run has consumed more than this (start_avail - avail)
# let a previous (killed) run's GPU memory drain before taking the baseline
prev=0; for i in $(seq 1 30); do
  cur=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
  [ "$cur" -le "$prev" ] && break; prev=$cur; sleep 2
done
START_AV=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
echo "START_AVAIL ${START_AV}GB MIN_AVAIL ${MIN_AVAIL_GB}GB MAX_USE ${MAX_USE_GB}GB" >> run.meta
/usr/bin/time -v -o time.txt "${CMD[@]}" > run.log 2> run.err &
RUNPID=$!
( while kill -0 $RUNPID 2>/dev/null; do
    av=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
    echo "$(date +%s.%N | cut -c1-14) $av $(awk '/MemAvailable/{print int($2/1024)}' /proc/meminfo) $(wc -l < run.log)" >> mem.trace
    if [ "$av" -lt "$MIN_AVAIL_GB" ] || [ $((START_AV - av)) -gt "$MAX_USE_GB" ]; then
      echo "WATCHDOG KILL avail=${av}GB at $(date -Is)" >> run.meta
      pkill -KILL -f "study_$CASE.ctr"; pkill -KILL -P $RUNPID; kill -KILL $RUNPID
      break
    fi
    sleep 0.2
  done ) &
WDPID=$!
wait $RUNPID; RC=$?
kill $WDPID 2>/dev/null
echo "RC $RC END $(date -Is)" >> run.meta
if [ "$MODE" = nsys ]; then
  /usr/local/cuda/bin/nsys stats -q --report cuda_gpu_kern_sum,cuda_gpu_mem_time_sum,cuda_gpu_mem_size_sum,nvtx_sum --format csv -o "$OUT/stats" "$OUT/prof.nsys-rep" > /dev/null 2>&1
fi
