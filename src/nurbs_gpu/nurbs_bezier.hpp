#pragma once
#include "nurbs_types.hpp"
#include <cmath>
#include <algorithm>
#include <vector>
#include <cassert>

namespace nurbs_gpu {

// Evaluate rational Bezier at t in [0,1] (de Casteljau, numerically stable).
inline void eval_bez(const BezSpan& b, f64 t, f64& x, f64& y) {
    int p = b.degree;
    f64 bx[4], by[4], bw[4];
    for (int i=0;i<=p;++i){ bx[i]=b.cx[i]*b.cw[i]; by[i]=b.cy[i]*b.cw[i]; bw[i]=b.cw[i]; }
    for (int r=1;r<=p;++r) for (int j=0;j<=p-r;++j){ 
        f64 u=1.0-t; bx[j]=u*bx[j]+t*bx[j+1]; by[j]=u*by[j]+t*by[j+1]; bw[j]=u*bw[j]+t*bw[j+1]; }
    x = bx[0]/bw[0]; y = by[0]/bw[0];
}

// Split rational Bezier at t into left/right (de Casteljau).
inline void split_bez(const BezSpan& b, f64 t, BezSpan& L, BezSpan& R) {
    int p=b.degree;
    f64 px[4][4], py[4][4], pw[4][4];
    for(int i=0;i<=p;++i){ px[0][i]=b.cx[i]; py[0][i]=b.cy[i]; pw[0][i]=b.cw[i]; }
    for(int r=1;r<=p;++r) for(int j=0;j<=p-r;++j){
        f64 u=1.0-t; px[r][j]=u*px[r-1][j]+t*px[r-1][j+1];
        py[r][j]=u*py[r-1][j]+t*py[r-1][j+1]; pw[r][j]=u*pw[r-1][j]+t*pw[r-1][j+1]; }
    L=b; R=b;
    for(int i=0;i<=p;++i){ L.cx[i]=px[i][0]; L.cy[i]=py[i][0]; L.cw[i]=pw[i][0];
                           R.cx[i]=px[p-i][i]; R.cy[i]=py[p-i][i]; R.cw[i]=pw[p-i][i]; }
}

// Derivative control points for polynomial (non-rational) Bezier degree p: p*(P_{i+1}-P_i)
inline void deriv_ctrl(const f64* c, int p, f64* dc) { for(int i=0;i<p;++i) dc[i]=p*(c[i+1]-c[i]); }

// Find t in (0,1) where y'(t)=0 for degree 1..3. Returns count, fills roots[].
inline int y_extrema(const BezSpan& b, f64 roots[4]) {
    int n=0, p=b.degree;
    if (p<=1) return 0;
    // For rational, use numerator of derivative: N'W - NW' = 0. Degree 2p-2.
    // Pragmatic: solve on polynomial approximation by subdividing or use homogeneous y*w derivative.
    // For degree<=3 rational, convert to polynomial in homogeneous: dy/dt of (Y/W) zero => Y'W - Y W' = 0
    // Build polynomial coefficients in Bernstein then find real roots in (0,1).
    // Simpler robust approach: sample+Newton refinement on Y'W-YW'. For degree<=2, quadratic.
    if (!b.flags || ((b.flags&1)==0)) { // non-rational
        f64 dc[3]; deriv_ctrl(b.cy, p, dc);
        if (p==2) { // linear: dc0 + t*(dc1-dc0)=0
            f64 den=dc[0]-dc[1]; if(std::abs(den)>1e-18){ f64 t=dc[0]/den; if(t>1e-12&&t<1-1e-12) roots[n++]=t; }
        } else if (p==3) { // quadratic Bernstein derivative
            f64 a=dc[0]-2*dc[1]+dc[2], bv=2*(dc[1]-dc[0]), c=dc[0];
            if(std::abs(a)<1e-18){ if(std::abs(bv)>1e-18){ f64 t=-c/bv; if(t>1e-12&&t<1-1e-12) roots[n++]=t; } }
            else { f64 disc=bv*bv-4*a*c; if(disc>=0){ disc=std::sqrt(disc); f64 t1=(-bv-disc)/(2*a), t2=(-bv+disc)/(2*a);
                if(t1>1e-12&&t1<1-1e-12) roots[n++]=t1; if(t2>1e-12&&t2<1-1e-12&&std::abs(t2-t1)>1e-12) roots[n++]=t2; } }
        }
        return n;
    }
    // Rational: evaluate f(t)=Y'(t)W(t)-Y(t)W'(t) via de Casteljau derivative formula
    auto f=[&](f64 t)->f64{
        // Use small-diff numerical derivative on homogeneous for robustness; replace with exact later.
        f64 eps=1e-7;
        f64 t0=std::max(0.0,t-eps), t1=std::min(1.0,t+eps);
        f64 x0,y0,x1,y1; eval_bez(b,t0,x0,y0); eval_bez(b,t1,x1,y1);
        return (y1-y0)/(t1-t0);
    };
    // Scan and bisection
    const int S=32;
    f64 prev=f(1e-12);
    for(int i=1;i<=S;++i){ f64 t=i/(f64)S; f64 cur=f(t);
        if(prev==0.0 || prev*cur<=0.0){ f64 lo=(i-1)/(f64)S, hi=t, flo=prev, fhi=cur;
            for(int it=0;it<60;++it){ f64 mid=0.5*(lo+hi), fm=f(mid); if(flo*fm<=0){hi=mid;fhi=fm;} else {lo=mid;flo=fm;} }
            f64 r=0.5*(lo+hi); if(r>1e-10&&r<1-1e-10) { bool dup=false; for(int k=0;k<n;++k) if(std::abs(roots[k]-r)<1e-8) dup=true; if(!dup) roots[n++]=r; }
        } prev=cur;
    }
    return n;
}

// Split curve at all Y-extrema so each piece is y-monotone.
inline void make_y_monotone(const BezSpan& in, std::vector<BezSpan>& out) {
    f64 roots[8]; int n=y_extrema(in, roots);
    std::sort(roots, roots+n);
    // dedupe
    std::vector<f64> ts; for(int i=0;i<n;++i) if(ts.empty()||roots[i]-ts.back()>1e-8) ts.push_back(roots[i]);
    std::vector<BezSpan> cur; cur.push_back(in);
    // split sequentially (adjust t for remaining right side)
    BezSpan rem=in; f64 offset=0; f64 last=0;
    out.clear();
    for(f64 t:ts){
        f64 local=(t-last)/(1.0-last);
        if(local<=1e-10||local>=1-1e-10) continue;
        BezSpan L,R; split_bez(rem, local, L, R);
        L.flags |= 2; out.push_back(L);
        rem=R; last=t;
    }
    rem.flags |= 2; out.push_back(rem);
}

} // namespace nurbs_gpu
