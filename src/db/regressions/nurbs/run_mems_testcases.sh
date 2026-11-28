#!/bin/bash
# MEMS curvilinear testcase regression: generate/verify NURBS-BRep-2D-v1 testcases.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
status=0
for f in "$HERE/testcases/"*.json; do
  python3 - <<PY || status=1
import json,sys
d=json.load(open('$f'))
assert d.get('schema')=='NURBS-BRep-2D-v1'
curves=d['curves']; topo=d.get('face_topology',{})
names=set(curves.keys())
for n in topo.get('outer_wire_ccw',[])+sum(topo.get('inner_hole_wires_cw',[]),[])+sum(topo.get('additional_closed_wires',[]),[]):
    assert n in names, n
print('PASS', '$f'.split('/')[-1], 'curves', len(curves))
PY
done
if [ $status -eq 0 ]; then echo "ALL PASS"; else echo "FAILURES"; fi
exit $status
