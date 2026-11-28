#include <cavc/polyline.hpp>
#include <cavc/polylineoffset.hpp>
#include <cstdio>
#include <vector>
#include <cmath>
#include <chrono>
using namespace cavc;
static double now_ms(){ using namespace std::chrono; return duration<double,std::milli>(high_resolution_clock::now().time_since_epoch()).count(); }
int main(int argc,char**argv){
    const char* path=argc>1?argv[1]:"poly.txt"; double d=argc>2?atof(argv[2]):1.0;
    FILE*f=fopen(path,"r"); if(!f){perror("open"); return 1;}
    Polyline<double> pl; std::vector<std::pair<double,double>> pts; char line[4096];
    while(fgets(line,sizeof(line),f)){
        if(line[0]=='#'||line[0]=='\n'){ continue; }
        double x,y; if(sscanf(line,"%lf %lf",&x,&y)==2) pts.push_back({x,y});
    }
    fclose(f);
    if(pts.size()>1 && std::hypot(pts.front().first-pts.back().first,pts.front().second-pts.back().second)<1e-9) pts.pop_back();
    for(auto&p:pts) pl.addVertex(p.first,p.second,0);
    pl.isClosed()=true;
    double t0=now_ms();
    auto res = parallelOffset(pl,d);
    double t1=now_ms();
    double area=0; for(auto& r:res){ area += getArea(r); }
    printf("cavc in_pts=%zu out_polys=%zu area=%.9f ms=%.3f\n", pts.size(), res.size(), area, t1-t0);
    // over-under
    double t2=now_ms();
    auto ou1=parallelOffset(pl,d); std::vector<Polyline<double>> ou2;
    for(auto& p:ou1){ auto t=parallelOffset(p,-d); ou2.insert(ou2.end(),t.begin(),t.end()); }
    double t3=now_ms();
    double aou=0; for(auto& r:ou2) aou+=getArea(r);
    printf("cavc OU_area=%.9f ms=%.3f\n", aou, t3-t2);
    return 0;
}
