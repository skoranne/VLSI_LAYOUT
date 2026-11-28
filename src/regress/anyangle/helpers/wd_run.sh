#!/bin/bash
# run_mag_rot.sh SDT16S with a MemAvailable watchdog (kills the MAGPARSER child by pid below 4 GB)
cd /home/skoranne/GITHUB/VLSI_LAYOUT/src/regress/anyangle
MAGPARSER=$1 ./run_mag_rot.sh SDT16S > out/mag_SDT16S.nohup 2>&1 &
RUN=$!
while kill -0 $RUN 2>/dev/null; do
  av=$(awk '/MemAvailable/ {print int($2/1048576)}' /proc/meminfo)
  if [ "$av" -lt 4 ]; then
    for p in $(pgrep -P $RUN); do for c in $(pgrep -P $p); do kill $c 2>/dev/null; done; done
    echo "WATCHDOG killed at MemAvailable=${av}G $(date)" >> out/mag_SDT16S.nohup
  fi
  sleep 5
done
