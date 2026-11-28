#!/usr/bin/env python3
import json, math, os, subprocess, sys
from pathlib import Path

def eval_nurbs(curve, t):
    pts=curve['control_points']; wts=curve.get('weights',[1.0]*len(pts)); p=curve['degree']
    n=len(pts)-1
    # Bernstein for clamped bezier
    def bern(nn,i,tt):
        from math import comb
        return comb(nn,i)*(tt**i)*((1-tt)**(nn-i))
    x=y=W=0.0
    for i in range(n+1):
        b=bern(n,i,t); x+=b*wts[i]*pts[i][0]; y+=b*wts[i]*pts[i][1]; W+=b*wts[i]
    return x/W,y/W

def sample(curve,ns=256):
    return [eval_nurbs(curve,i/ns) for i in range(ns+1)]

def wire_pts(names, curves, ns=256):
    pts=[]
    for name in names:
        sp=sample(curves[name],ns)
        if pts and math.hypot(pts[-1][0]-sp[0][0], pts[-1][1]-sp[0][1])<1e-9: pts.extend(sp[1:])
        else: pts.extend(sp)
    if pts and math.hypot(pts[0][0]-pts[-1][0], pts[0][1]-pts[-1][1])<1e-9: pts=pts[:-1]
    return pts

def write_poly(path, polys):
    with open(path,'w') as f:
        for i,poly in enumerate(polys):
            f.write(f'# poly{i}\n')
            for x,y in poly: f.write(f'{x:.9f} {y:.9f}\n')
            f.write('\n')

def poly_area(pts):
    a=0.0
    for i in range(len(pts)):
        x1,y1=pts[i]; x2,y2=pts[(i+1)%len(pts)]; a+=x1*y2-x2*y1
    return 0.5*a

def main():
    tc_dir=Path(sys.argv[1]); work=Path(sys.argv[2]); d=float(sys.argv[3]) if len(sys.argv)>3 else 1.0
    work.mkdir(parents=True, exist_ok=True)
    clip='/scratch1/skoranne/CURVILINEAR_KERNELS/bench/mems_testcases/clipper_offset'
    ref={}
    for jf in sorted(tc_dir.glob('*.json')):
        dct=json.load(open(jf))
        curves=dct['curves']; topo=dct.get('face_topology',{})
        outer=wire_pts(topo.get('outer_wire_ccw',[]), curves, 256)
        holes=[wire_pts(h,curves,256) for h in topo.get('inner_hole_wires_cw',[])]
        polys=[outer]+holes
        poly_path=work/(jf.stem+'.poly')
        write_poly(poly_path, polys)
        out_txt=work/(jf.stem+'.ref.txt')
        cmd=[clip,str(poly_path),str(d),str(out_txt)]
        r=subprocess.run(cmd,capture_output=True,text=True)
        if r.returncode!=0:
            print('ERR',jf.name,r.stderr,r.stdout); sys.exit(1)
        vals={}
        for line in open(out_txt):
            k,v=line.strip().split()
            vals[k]=float(v) if '.' in v else int(v)
        ref[jf.stem]={
            'orig':{'area':vals['orig_area'],'perim':vals['orig_perim'],'paths':vals['orig_paths']},
            'over_under':{'area':vals['over_under_area'],'perim':vals['over_under_perim'],'paths':vals['over_under_paths']},
            'under_over':{'area':vals['under_over_area'],'perim':vals['under_over_perim'],'paths':vals['under_over_paths']},
            'd':d
        }
        # MAGPARSER input: solid paths with hole sign preserved (CW negative = hole)
        scale=1000.0
        solid='/scratch1/skoranne/CURVILINEAR_KERNELS/bench/mems_testcases/clipper_solid'
        solid_poly=work/(jf.stem+'_solid.poly')
        subprocess.run([solid,str(poly_path),str(solid_poly)],check=True)
        sp=[]; cur=[]
        for ln in open(solid_poly):
            if ln.startswith('#') or ln.strip()=='':
                if cur: sp.append(cur); cur=[]
            else:
                x,y=ln.split(); cur.append((float(x),float(y)))
        if cur: sp.append(cur)
        mf=work/(jf.stem+'.magpoly')
        with open(mf,'w') as f:
            f.write(f'{len(sp)}\n')
            for poly_s in sp:
                f.write(f'{len(poly_s)}\n')
                for x,y in poly_s: f.write(f'{int(round(x*scale))} {int(round(y*scale))}\n')
        print(jf.stem, 'orig_area=%.3f'%vals['orig_area'], 'OU_area=%.3f'%vals['over_under_area'], 'UO_area=%.3f'%vals['under_over_area'])
    json.dump(ref,open(work/'ref.json','w'),indent=2)
    print('wrote',work/'ref.json')

if __name__=='__main__': main()
