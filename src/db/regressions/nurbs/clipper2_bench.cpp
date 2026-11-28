#include <clipper2/clipper.h>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <string>
#include <chrono>
#include <cmath>
using namespace Clipper2Lib;
static double now_ms(){ using namespace std::chrono; return duration<double,std::milli>(high_resolution_clock::now().time_since_epoch()).count(); }
int main(int argc,char**argv){
    const char* path = argc>1?argv[1]:"poly.txt";
    int scaleExp = argc>2?atoi(argv[2]):6; // scale 10^6 for um
    double scale = std::pow(10.0, scaleExp);
    FILE*f=fopen(path,"r"); if(!f){perror("open"); return 1;}
    PathsD subj; PathD cur; char line[4096];
    while(fgets(line,sizeof(line),f)){
        if(line[0]=='#'||line[0]=='\n'){ if(!cur.empty()){subj.push_back(cur); cur.clear();} continue; }
        double x,y; if(sscanf(line,"%lf %lf",&x,&y)==2) cur.push_back(PointD(x,y));
    }
    if(!cur.empty()) subj.push_back(cur);
    fclose(f);
    // assume first is outer, rest holes for curve1; just union as-is
    double t0=now_ms();
    PathsD sol = Union(subj, FillRule::NonZero);
    double t1=now_ms();
    double area=0; double per=0;
    for(auto&p:sol){ area+=Area(p); per+=Length(p); }
    printf("clipper2 paths_in=%zu paths_out=%zu area=%.9f perim=%.9f union_ms=%.3f\n", subj.size(), sol.size(), area, per, t1-t0);
    // offset test
    double t2=now_ms();
    PathsD off = InflatePaths(sol, 1.5, JoinType::Round, EndType::Polygon);
    double t3=now_ms();
    double a2=0; for(auto&p:off) a2+=Area(p);
    printf("clipper2 offset_d=1.5 paths=%zu area=%.9f offset_ms=%.3f\n", off.size(), a2, t3-t2);
    return 0;
}
