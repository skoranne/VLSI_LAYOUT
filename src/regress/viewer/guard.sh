#!/bin/bash
# guard.sh LOG cmd args... : run cmd with output to LOG; kill it if MemAvailable drops below
# GUARD_MIN_GB (default 24). Appends "RC rc START_AVAIL a MIN_AVAIL m RSS_KB r WALL w" to LOG.
log=$1; shift
min=${GUARD_MIN_GB:-24}
start=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
/usr/bin/time -f "RSS_KB %M WALL %e" -o "$log.time" "$@" > "$log" 2>&1 &
pid=$!
minav=$start
while kill -0 $pid 2>/dev/null; do
  av=$(awk '/MemAvailable/{print int($2/1048576)}' /proc/meminfo)
  [ "$av" -lt "$minav" ] && minav=$av
  if [ "$av" -lt "$min" ]; then echo "WATCHDOG KILL avail=${av}GB" >> "$log"; pkill -9 -P $pid; kill -9 $pid; fi
  sleep 1
done
wait $pid; rc=$?
echo "RC $rc START_AVAIL ${start}GB MIN_AVAIL ${minav}GB $(tail -1 "$log.time")" >> "$log"
exit $rc
