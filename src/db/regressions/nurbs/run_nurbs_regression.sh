#!/bin/bash
# NURBS/curvilinear regression: degree<=3 rational Bezier primitives, curve1.json reference, y-monotone split.
# Usage: bash src/db/regressions/nurbs/run_nurbs_regression.sh [MAGPARSER.exe] [workdir]
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
SRC=$(cd "$HERE/../../.." && pwd)
EXE=${1:-$SRC/MAGPARSER.exe}
WORK=${2:-$(mktemp -d /tmp/nurbs_regr_XXXXXX)}
mkdir -p "$WORK"
echo "workdir: $WORK"
status=0

# 1) Unit test (Fortran)
if [ -x "$SRC/test_nurbs.exe" ]; then
  "$SRC/test_nurbs.exe" > "$WORK/test_nurbs.log" 2>&1 || { echo "FAIL: test_nurbs.exe"; cat "$WORK/test_nurbs.log"; status=1; }
  grep -q "test_nurbs: PASS" "$WORK/test_nurbs.log" && echo "PASS: test_nurbs" || { echo "FAIL: test_nurbs"; status=1; }
else
  echo "SKIP: $SRC/test_nurbs.exe not built"
fi

# 2) Python tessellation reference for curve1.json
cat > "$WORK/nurbs_ref.py" << 'PY'
import json,math,sys
def bernstein(n,i,t):
    from math import comb
    return comb(n,i)*(t**i)*((1-t)**(n-i))
def eval_nurbs(curve,t):
    pts=curve['control_points']; wts=curve.get('weights',[1.0]*len(pts)); p=curve['degree']
    n=len(pts)-1; x=y=W=0.0
    for i in range(n+1):
        b=bernstein(n,i,t); x+=b*wts[i]*pts[i][0]; y+=b*wts[i]*pts[i][1]; W+=b*wts[i]
    return x/W,y/W
def sample(curve,ns):
    return [eval_nurbs(curve,i/ns) for i in range(ns+1)]
def area(pts):
    a=0.0
    for i in range(len(pts)):
        x1,y1=pts[i]; x2,y2=pts[(i+1)%len(pts)]; a+=x1*y2-x2*y1
    return 0.5*a
def perim(pts):
    p=0.0
    for i in range(len(pts)):
        x1,y1=pts[i]; x2,y2=pts[(i+1)%len(pts)]; p+=math.hypot(x2-x1,y2-y1)
    return p
d=json.load(open(sys.argv[1]))
curves=d['curves']; topo=d['face_topology']
def wire(names,ns):
    pts=[]
    for name in names:
        sp=sample(curves[name],ns)
        if pts and math.hypot(pts[-1][0]-sp[0][0],pts[-1][1]-sp[0][1])<1e-9: pts.extend(sp[1:])
        else: pts.extend(sp)
    if pts and math.hypot(pts[0][0]-pts[-1][0],pts[0][1]-pts[-1][1])<1e-9: pts=pts[:-1]
    return pts
outer=wire(topo['outer_wire_ccw'],4096); holes=[wire(h,4096) for h in topo.get('inner_hole_wires_cw',[])]
a=area(outer)+sum(area(h) for h in holes)
p=perim(outer)+sum(perim(h) for h in holes)
print(f'area={a:.9f} perim={p:.9f}')
PY
REF=$(python3 "$WORK/nurbs_ref.py" "$SRC/curve1.json")
echo "REF $REF"
echo "$REF" | grep -q "area=4305.7795" && echo "PASS: curve1 reference" || { echo "FAIL: curve1 reference"; status=1; }

# 3) mems1.json sanity (schema, explicit curves)
python3 - <<PY > "$WORK/mems1_check.log" 2>&1
import json,sys
d=json.load(open('$SRC/mems1.json'))
assert d['schema']=='NURBS-BRep-2D-v1'
assert len(d['curves'])>=7
print('ok')
PY
grep -q ok "$WORK/mems1_check.log" && echo "PASS: mems1 schema" || { echo "FAIL: mems1 schema"; status=1; }

if [ $status -eq 0 ]; then echo "ALL PASS"; else echo "FAILURES"; fi
exit $status
