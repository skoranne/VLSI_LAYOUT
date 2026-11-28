#include "nurbs_bezier.hpp"
#include "nurbs_bool_cpu.hpp"
#include <cstdio>
using namespace nurbs_gpu;
int main(){
    // degree-2 rational quarter circle approximation as Bezier: (1,0),(1,1),(0,1) w=1,1/sqrt(2),1
    BezSpan q{}; q.degree=2; q.flags=1; q.owner=0;
    q.cx[0]=1; q.cy[0]=0; q.cw[0]=1;
    q.cx[1]=1; q.cy[1]=1; q.cw[1]=0.7071067811865476;
    q.cx[2]=0; q.cy[2]=1; q.cw[2]=1;
    std::vector<BezSpan> mono; make_y_monotone(q, mono);
    printf("mono_spans=%zu\n", mono.size());
    double a=approx_area(mono, 100000); // not closed; just sanity
    double x0,y0,x1,y1; eval_bez(q,0,x0,y0); eval_bez(q,1,x1,y1);
    printf("p0=(%.6f %.6f) p1=(%.6f %.6f) rough=%g\n",x0,y0,x1,y1,a);
    // full circle area from 4 arcs
    BezSpan c[4]={};
    double w=0.7071067811865476;
    double pts[9][2]={{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1},{1,0}};
    double ws[9]={1,w,1,w,1,w,1,w,1};
    double area=0;
    for(int k=0;k<4;++k){ BezSpan b{}; b.degree=2; b.flags=1; b.owner=1; for(int j=0;j<3;++j){b.cx[j]=pts[2*k+j][0];b.cy[j]=pts[2*k+j][1];b.cw[j]=ws[2*k+j];}
        std::vector<BezSpan> ms; make_y_monotone(b,ms);
        for(auto&s:ms){ for(int i=0;i<200000;++i){ double t0=i/200000.0,t1=(i+1)/200000.0; double ax,ay,bx,by; eval_bez(s,t0,ax,ay); eval_bez(s,t1,bx,by); area+=ax*by-bx*ay; } }
    }
    printf("circle_area_approx=%.12f (pi=%.12f)\n", 0.5*area, M_PI);
    return 0;
}
