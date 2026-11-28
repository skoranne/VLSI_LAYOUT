#!/usr/bin/env python3
"""Generate curvilinear MEMS NURBS-BRep-2D-v1 testcases and save to disk."""
import json, math
from pathlib import Path

OUT_SCRATCH = Path('/scratch1/skoranne/CURVILINEAR_KERNELS/bench/mems_testcases')
OUT_REPO = Path('/home/skoranne/GITHUB/VLSI_LAYOUT/src/db/regressions/nurbs/testcases')
OUT_SCRATCH.mkdir(parents=True, exist_ok=True)
OUT_REPO.mkdir(parents=True, exist_ok=True)
WQ = math.sqrt(2) / 2

def circle_spans(cx, cy, r, name, ccw=True):
    pts=[(r,0),(r,r),(0,r),(-r,r),(-r,0),(-r,-r),(0,-r),(r,-r),(r,0)]
    ws=[1,WQ,1,WQ,1,WQ,1,WQ,1]
    spans={}; order=list(range(4)) if ccw else list(range(3,-1,-1))
    for qi in order:
        i=2*qi
        p0=(cx+pts[i][0],cy+pts[i][1]); p1=(cx+pts[i+1][0],cy+pts[i+1][1]); p2=(cx+pts[i+2][0],cy+pts[i+2][1])
        w0,w1,w2=ws[i],ws[i+1],ws[i+2]
        if not ccw: p0,p2=p2,p0; w0,w2=w2,w0
        spans[f'{name}_q{qi}']={'type':'NURBS_CURVE','degree':2,'rational':True,'knots':[0,0,0,1,1,1],
                                 'control_points':[list(p0),list(p1),list(p2)],'weights':[w0,w1,w2]}
    return spans, list(spans.keys())

def line_span(p0,p1,name):
    return name, {'type':'NURBS_CURVE','degree':1,'rational':False,'knots':[0,0,1,1],
                  'control_points':[list(p0),list(p1)],'weights':[1.0,1.0]}

def cubic_span(p0,p1,p2,p3,name,w=None):
    w=w or [1.0,1.0,1.0,1.0]
    return name, {'type':'NURBS_CURVE','degree':3,'rational':any(abs(x-1)>1e-12 for x in w),
                  'knots':[0,0,0,0,1,1,1,1],'control_points':[list(p0),list(p1),list(p2),list(p3)],'weights':list(w)}

def arc90(cx,cy,r,a0_deg,name,ccw=True):
    a0=math.radians(a0_deg); a1=a0 + (math.pi/2 if ccw else -math.pi/2); am=a0 + (math.pi/4 if ccw else -math.pi/4)
    p0=(cx+r*math.cos(a0), cy+r*math.sin(a0)); p2=(cx+r*math.cos(a1), cy+r*math.sin(a1))
    pm=(cx+(r/math.cos(math.pi/4))*math.cos(am), cy+(r/math.cos(math.pi/4))*math.sin(am))
    w1=math.cos(math.pi/4)
    return name, {'type':'NURBS_CURVE','degree':2,'rational':True,'knots':[0,0,0,1,1,1],
                  'control_points':[list(p0),list(pm),list(p2)],'weights':[1.0,w1,1.0]}

def polyline_span(pts,name):
    cp=list(pts)
    if abs(cp[0][0]-cp[-1][0])>1e-12 or abs(cp[0][1]-cp[-1][1])>1e-12: cp.append(cp[0])
    n=len(cp)-1
    knots=[0,0]+list(range(1,n))+[n,n]
    return name, {'type':'NURBS_POLYLINE','degree':1,'rational':False,'knots':knots,
                  'control_points':[list(p) for p in cp],'weights':[1.0]*len(cp)}

def write_case(name, doc):
    for base in [OUT_SCRATCH, OUT_REPO]:
        p=base/f'{name}.json'
        p.write_text(json.dumps(doc, indent=2))
        print('wrote', p)

def rot(p,ang): c,s=math.cos(ang),math.sin(ang); return (c*p[0]-s*p[1], s*p[0]+c*p[1])

def gen_gear():
    z=12; m=10.0; alpha=math.radians(20); rb=m*z/2*math.cos(alpha)
    ra=m*z/2+m; rf=m*z/2-1.25*m
    def involute(r_b,t): return (r_b*(math.cos(t)+t*math.sin(t)), r_b*(math.sin(t)+t*math.cos(t)))
    def inv_angle(r,r_b): return 0.0 if r<=r_b else math.sqrt(max(0,(r/r_b)**2-1))
    t0=inv_angle(max(rf,rb),rb); t1=inv_angle(ra,rb)
    tp=inv_angle(m*z/2,rb); pp=involute(rb,tp); ang_p=math.atan2(pp[1],pp[0])-tp
    tooth_half=math.pi/z; pitch_off=tooth_half - (math.pi/(2*z)+math.tan(alpha)-alpha)
    pts=[]
    for i in range(z):
        ang=2*math.pi*i/z
        flank=[]
        for j in range(10):
            u=j/9; tt=t0+u*(t1-t0); p=involute(rb,tt); p=rot(p,-ang_p+pitch_off); flank.append(rot(p,ang))
        # tip arc (approx)
        a_s=math.atan2(flank[-1][1],flank[-1][0])
        # mirror flank
        flank2=[]
        for j in range(10):
            u=j/9; tt=t0+u*(t1-t0); p=involute(rb,tt); p=(p[0],-p[1]); p=rot(p,ang_p-pitch_off); flank2.append(rot(p,ang))
        a_e=math.atan2(flank2[-1][1],flank2[-1][0])
        # root arc next
        a_n=2*math.pi*(i+1)/z
        # build tip arc points
        tip=[]
        for j in range(6):
            aa=a_s + ( (a_e-a_s) % (2*math.pi) - 2*math.pi*0.0 )*j/5
        # simpler: just append flank, tip arc by angle sweep, flank2 reversed, root arc
        pts.extend(flank)
        # tip
        b0=math.atan2(flank[-1][1],flank[-1][0]); b1=math.atan2(flank2[0][1],flank2[0][0])
        db=(b1-b0)%(2*math.pi)
        if db>math.pi: db-=2*math.pi
        for j in range(1,6): aa=b0+db*j/6; pts.append((ra*math.cos(aa), ra*math.sin(aa)))
        pts.extend(flank2)
        # root arc to next tooth
        n_ang=2*math.pi*((i+1)%z)/z
        # next tooth start
        p_next=involute(rb,t0); p_next=rot(p_next,-ang_p+pitch_off); p_next=rot(p_next,n_ang)
        b0=math.atan2(flank2[-1][1],flank2[-1][0]); b1=math.atan2(p_next[1],p_next[0])
        db=(b1-b0)%(2*math.pi)
        if db>math.pi: db-=2*math.pi
        for j in range(1,6): aa=b0+db*j/6; pts.append((rf*math.cos(aa), rf*math.sin(aa)))
    cleaned=[pts[0]]
    for p in pts[1:]:
        if math.hypot(p[0]-cleaned[-1][0],p[1]-cleaned[-1][1])>1e-6: cleaned.append(p)
    if math.hypot(cleaned[0][0]-cleaned[-1][0],cleaned[0][1]-cleaned[-1][1])<1e-6: cleaned=cleaned[:-1]
    curves={}; n,sp=polyline_span(cleaned,'gear_outer'); curves[n]=sp
    bore,bw=circle_spans(0,0,15.0,'bore',ccw=False); curves.update(bore)
    holes=[bw]
    for k in range(6):
        ang=2*math.pi*k/6; h,hw=circle_spans(35*math.cos(ang),35*math.sin(ang),6.0,f'hole{k}',ccw=False)
        curves.update(h); holes.append(hw)
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'involute_spur_gear',
         'parameters':{'z':z,'m_um':m,'alpha_deg':20,'rb':rb,'ra':ra,'rf':rf},
         'curves':curves,'face_topology':{'outer_wire_ccw':['gear_outer'],'inner_hole_wires_cw':holes}}
    write_case('gear_involute',doc)

def gen_comb_actuator():
    # Linear comb-drive actuator: proof mass with etch holes + 2 comb banks + folded springs approximated as curvilinear boundary
    curves={}; holes=[]
    # outer shuttle body
    body=[(-120,-30),(120,-30),(120,30),(-120,30)]
    n,sp=polyline_span(body,'shuttle_body'); curves[n]=sp
    # etch holes in shuttle
    for ix,x in enumerate(range(-90,91,30)):
        for iy,y in enumerate(range(-15,16,30)):
            h,hw=circle_spans(x,y,5.0,f'etch_{ix}_{iy}',ccw=False); curves.update(h); holes.append(hw)
    # comb fingers as separate closed polylines with semicircular tips (approximated by arcs)
    finger_w=2.0; gap=1.8; pitch=finger_w+gap; nf=20; finger_len=25.0
    for side,sy in [('top',30.0),('bot',-30.0)]:
        for i in range(nf):
            x0=-nf*pitch/2 + i*pitch
            dir=1 if side=='top' else -1
            y1=sy; y2=sy+dir*finger_len
            # rounded tip: semicircle
            cxm=x0+finger_w/2; r=finger_w/2
            pts=[(x0,y1),(x0,y2),(x0+finger_w,y2),(x0+finger_w,y1)]
            n2,sp2=polyline_span(pts,f'finger_{side}_{i}'); curves[n2]=sp2
    # folded spring (U shape with circular bends)
    spring=[(-150,-30),(-170,-30),(-170,-60),(-130,-60),(-130,-40),(-150,-40)]
    n,sp=polyline_span(spring,'spring_l'); curves[n]=sp
    spring2=[(150,-30),(170,-30),(170,-60),(130,-60),(130,-40),(150,-40)]
    n,sp=polyline_span(spring2,'spring_r'); curves[n]=sp
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'linear_comb_actuator',
         'parameters':{'finger_w_um':finger_w,'gap_um':gap,'N_fingers':nf,'finger_len_um':finger_len},
         'curves':curves,
         'face_topology':{'outer_wire_ccw':['shuttle_body'],'inner_hole_wires_cw':holes,
                          'additional_closed_wires':[[f'finger_top_{i}'] for i in range(nf)] + [[f'finger_bot_{i}'] for i in range(nf)] + [['spring_l'],['spring_r']]}}
    write_case('actuator_comb_linear',doc)

def gen_gyro():
    curves={}; holes=[]
    # two proof masses
    for side,cx in [('L',-80),('R',80)]:
        body=[(cx-40,-30),(cx+40,-30),(cx+40,30),(cx-40,30)]
        n,sp=polyline_span(body,f'mass_{side}'); curves[n]=sp
        for ix,dx in enumerate(range(-25,26,25)):
            for iy,dy in enumerate(range(-15,16,30)):
                h,hw=circle_spans(cx+dx,dy,5.0,f'etch_{side}_{ix}_{iy}',ccw=False); curves.update(h); holes.append(hw)
    # coupling spring - cubic spline
    n,sp=cubic_span((-40,0),(-15,40),(15,-40),(40,0),'coupling_spring'); curves[n]=sp
    # anchor ring
    ring,ringw=circle_spans(0,0,12.0,'anchor_outer',ccw=True); curves.update(ring)
    ringh,ringhw=circle_spans(0,0,6.0,'anchor_inner',ccw=False); curves.update(ringh); holes.append(ringhw)
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'tuning_fork_gyroscope',
         'parameters':{'mass_w_um':80,'mass_h_um':60,'coupling':'cubic_spline'},
         'curves':curves,
         'face_topology':{'outer_wire_ccw':['mass_L'],'inner_hole_wires_cw':[h for h in holes if h[0].startswith('etch_L')],
                          'additional_closed_wires':[['mass_R'], ringw, ['coupling_spring']]}}
    write_case('gyro_tuning_fork',doc)

def gen_accelerometer():
    curves={}; holes=[]
    # central proof mass
    mass=[(-60,-60),(60,-60),(60,60),(-60,60)]
    n,sp=polyline_span(mass,'proof_mass'); curves[n]=sp
    # hex-packed circular etch holes
    hid=0
    for row,y in enumerate(range(-45,46,15)):
        off = 7.5 if row%2 else 0.0
        for x in range(-45,46,15):
            xx=x+off
            if abs(xx)>50 or abs(y)>50: continue
            h,hw=circle_spans(xx,y,4.0,f'etch_{hid}',ccw=False); curves.update(h); holes.append(hw); hid+=1
    # 4 U-springs at corners with circular bend approximation
    spring_pts=[(60,50),(90,50),(90,70),(70,70),(70,60),(60,60)]
    n,sp=polyline_span(spring_pts,'spring_ur'); curves[n]=sp
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'differential_accelerometer',
         'parameters':{'mass_um':120,'etch_r_um':4.0,'etch_count':hid},
         'curves':curves,'face_topology':{'outer_wire_ccw':['proof_mass'],'inner_hole_wires_cw':holes,
                                          'additional_closed_wires':[['spring_ur']]}}
    write_case('accel_differential',doc)

def gen_mirror():
    curves={}; holes=[]
    # elliptical mirror plate (4-span degree-2 rational)
    rx,ry=150.0,120.0
    pts=[(rx,0),(rx,ry),(0,ry),(-rx,ry),(-rx,0),(-rx,-ry),(0,-ry),(rx,-ry),(rx,0)]
    ws=[1,WQ,1,WQ,1,WQ,1,WQ,1]
    spans={}; 
    for qi in range(4):
        i=2*qi
        p0=(pts[i][0],pts[i][1]); p1=(pts[i+1][0],pts[i+1][1]); p2=(pts[i+2][0],pts[i+2][1])
        spans[f'mirror_q{qi}']={'type':'NURBS_CURVE','degree':2,'rational':True,'knots':[0,0,0,1,1,1],
                                'control_points':[list(p0),list(p1),list(p2)],'weights':[ws[i],ws[i+1],ws[i+2]]}
    curves.update(spans)
    # gimbal ring outer/inner
    g_out,gw_out=circle_spans(0,0,190.0,'gimbal_out',ccw=True); curves.update(g_out)
    g_in,gw_in=circle_spans(0,0,165.0,'gimbal_in',ccw=False); curves.update(g_in); holes.append(gw_in)
    # torsion hinge
    hinge=[(-5,150),(5,150),(3,165),(-3,165)]
    n,sp=polyline_span(hinge,'hinge_n'); curves[n]=sp
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'gimbal_micromirror_2axis',
         'parameters':{'mirror_rx_um':rx,'mirror_ry_um':ry,'gimbal_r_out_um':190,'gimbal_r_in_um':165},
         'curves':curves,
         'face_topology':{'outer_wire_ccw':list(spans.keys()),'inner_hole_wires_cw':[],
                          'additional_closed_wires':[gw_out, ['hinge_n']]}}
    write_case('mirror_gimbal_2axis',doc)

def gen_flux_concentrator():
    curves={}
    # tapered cubic NURBS horn narrowing to gap
    n,sp=cubic_span((-100,-40),(-40,-10),(-10,-2),(-2,-0.5),'horn_l_bot'); curves[n]=sp
    n,sp=cubic_span((-2,-0.5),(0,-0.5),(0,0.5),(-2,0.5),'gap_bot'); curves[n]=sp
    n,sp=cubic_span((-2,0.5),(-10,2),(-40,10),(-100,40),'horn_l_top'); curves[n]=sp
    n,sp=line_span((-100,40),(-100,-40),'horn_l_back'); curves[n]=sp
    # right horn mirror
    n,sp=cubic_span((100,-40),(40,-10),(10,-2),(2,-0.5),'horn_r_bot'); curves[n]=sp
    n,sp=cubic_span((2,-0.5),(2,0.5),(2,0.5),(2,0.5),'gap_r_stub'); curves[n]=sp
    n,sp=cubic_span((2,0.5),(10,2),(40,10),(100,40),'horn_r_top'); curves[n]=sp
    n,sp=line_span((100,40),(100,-40),'horn_r_back'); curves[n]=sp
    # fix gap_r_stub as line
    curves['gap_r_stub']={'type':'NURBS_CURVE','degree':1,'rational':False,'knots':[0,0,1,1],
                          'control_points':[[2,-0.5],[2,0.5]],'weights':[1.0,1.0]}
    doc={'schema':'NURBS-BRep-2D-v1','units':'micrometer','tolerance':1e-6,'device':'mag_flux_concentrator',
         'parameters':{'gap_um':1.0,'horn_len_um':100},
         'curves':curves,
         'face_topology':{'outer_wire_ccw':['horn_l_bot','gap_bot','horn_l_top','horn_l_back'],
                          'inner_hole_wires_cw':[],
                          'additional_closed_wires':[['horn_r_bot','gap_r_stub','horn_r_top','horn_r_back']]}}
    write_case('mag_flux_concentrator',doc)

def main():
    gen_gear()
    gen_comb_actuator()
    gen_gyro()
    gen_accelerometer()
    gen_mirror()
    gen_flux_concentrator()
    print('done', len(list(OUT_REPO.glob('*.json'))), 'repo json files')

if __name__=='__main__':
    main()
