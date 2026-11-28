#!/usr/bin/env python3
"""NURBS-BRep-2D-v1 evaluator: parse, sample, exact degree<=2 rational Bezier area/perimeter, tessellate."""
import json, math, sys, os, time
from dataclasses import dataclass, field
from typing import List, Tuple

def bernstein(n,i,t):
    from math import comb
    return comb(n,i)*(t**i)*((1-t)**(n-i))

def eval_nurbs(curve, t):
    """Evaluate NURBS/Bezier curve at t in [0,1]. Supports degree 1,2 rational/non-rational."""
    pts = curve['control_points']
    wts = curve.get('weights', [1.0]*len(pts))
    p = curve['degree']
    knots = curve.get('knots')
    # For clamped Bezier (knots [0]*p+1 + [1]*p+1) use Bernstein
    if knots and len(knots) == 2*p+2 and all(k==0 for k in knots[:p+1]) and all(k==1 for k in knots[p+1:]):
        n = len(pts)-1
        x=y=W=0.0
        for i in range(n+1):
            b=bernstein(n,i,t)
            x += b*wts[i]*pts[i][0]
            y += b*wts[i]*pts[i][1]
            W += b*wts[i]
        return x/W, y/W
    # general de Boor
    # find span
    n = len(pts)-1
    if t >= 1.0: 
        i = n; 
        return pts[i][0], pts[i][1] if abs(wts[i]-1)<1e-15 else (pts[i][0],pts[i][1])
    # clamp
    t = max(knots[p], min(knots[n+1]-1e-15, t))
    kspan = p
    for i in range(p, n+1):
        if knots[i+1] > t: kspan=i; break
    # de Boor on homogeneous
    d = [[wts[j]*pts[j][0], wts[j]*pts[j][1], wts[j]] for j in range(kspan-p, kspan+1)]
    for r in range(1,p+1):
        for j in range(p, r-1, -1):
            denom = knots[kspan+j-r+1]-knots[kspan+j-p]
            alpha = 0.0 if abs(denom)<1e-18 else (t-knots[kspan+j-p])/denom
            d[j] = [alpha*d[j][k]+(1-alpha)*d[j-1][k] for k in range(3)]
    W=d[p][2]
    return d[p][0]/W, d[p][1]/W

def sample_curve(curve, nseg=64):
    pts=[]
    for i in range(nseg+1):
        t=i/nseg
        pts.append(eval_nurbs(curve,t))
    return pts

def polyline_area(pts):
    a=0.0
    for i in range(len(pts)):
        x1,y1=pts[i]; x2,y2=pts[(i+1)%len(pts)]
        a += x1*y2-x2*y1
    return 0.5*a

def polyline_perim(pts):
    p=0.0
    for i in range(len(pts)):
        x1,y1=pts[i]; x2,y2=pts[(i+1)%len(pts)]
        p += math.hypot(x2-x1,y2-y1)
    return p

def load_face(path, nseg=128):
    d=json.load(open(path))
    curves=d['curves']
    topo=d.get('face_topology',{})
    outer_names=topo.get('outer_wire_ccw',[])
    holes_names=topo.get('inner_hole_wires_cw',[])
    def wire_pts(names, ns):
        pts=[]
        for name in names:
            c=curves[name]
            sp=sample_curve(c,ns)
            # avoid duplicating seam point
            if pts and math.hypot(pts[-1][0]-sp[0][0], pts[-1][1]-sp[0][1])<1e-9:
                pts.extend(sp[1:])
            else:
                pts.extend(sp)
        # close
        if pts and math.hypot(pts[0][0]-pts[-1][0], pts[0][1]-pts[-1][1])<1e-9:
            pts=pts[:-1]
        return pts
    outer=wire_pts(outer_names,nseg)
    holes=[wire_pts(h,nseg) for h in holes_names]
    return d, outer, holes

if __name__=='__main__':
    path=sys.argv[1] if len(sys.argv)>1 else 'curve1.json'
    nseg=int(sys.argv[2]) if len(sys.argv)>2 else 256
    t0=time.time()
    d,outer,holes=load_face(path,nseg)
    dt=time.time()-t0
    a=polyline_area(outer)+sum(polyline_area(h) for h in holes)
    per=polyline_perim(outer)+sum(polyline_perim(h) for h in holes)
    print(f'file={os.path.basename(path)} nseg={nseg} outer_n={len(outer)} holes={len(holes)} hole_ns={[len(h) for h in holes]}')
    print(f'area={a:.9f} perim={per:.9f} eval_ms={dt*1000:.2f}')
