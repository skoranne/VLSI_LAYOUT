#include "layoutdb.h"
#include "lefdef_export.h"
#include <cstdio>
#include <cstdlib>
#include <cinttypes>
#include <string>
#include <vector>
#include <cstring>

static void usage(const char* a){
    fprintf(stderr,
      "Usage: %s <input.oas|input.gds> <out.lef> <out.def> [options]\n"
      "Options:\n"
      "  --design NAME           DEF design name (default: TOP)\n"
      "  --dbu N                 UNITS DISTANCE MICRONS (default: 1000)\n"
      "  --connect \"LIST\"        CONNECT stack names (default: LI1 VIA MET1 VIA1 MET2 VIA2 MET3 VIA3 MET4 VIA4 MET5)\n"
      "  --map NAME=L:D[:TYPE]   map GDS layer:datatype to NAME with TYPE ROUTING|CUT (repeatable)\n"
      "                          default maps use Sky130-like CONNECT stack\n"
      "  --blockages             emit BLOCKAGES instead of SPECIALNETS\n"
      "Implements OASIS/GDSII -> LEF/DEF export using CONNECT stack (e.g. LI1 VIA MET1 VIA1 MET2 VIA2 MET3 VIA3 MET4 VIA4 MET5).\n",
      a);
}

static std::vector<std::string> split_ws(const std::string& s){
    std::vector<std::string> out; std::string cur;
    for(char c: s){ if(isspace((unsigned char)c)){ if(!cur.empty()){out.push_back(cur); cur.clear();} } else cur.push_back(c); }
    if(!cur.empty()) out.push_back(cur); return out;
}

int main(int argc, char** argv){
    if(argc < 4){ usage(argv[0]); return 2; }
    std::string in=argv[1], lef=argv[2], def=argv[3];
    LefDefOptions opt;
    opt.layer_map = default_sky130_connect_map();
    opt.connect_stack = {"LI1","VIA","MET1","VIA1","MET2","VIA2","MET3","VIA3","MET4","VIA4","MET5"};
    bool custom_map = false;

    for(int i=4;i<argc;i++){
        std::string a=argv[i];
        if(a=="--design" && i+1<argc){ opt.design_name=argv[++i]; }
        else if(a=="--dbu" && i+1<argc){ opt.dbu_per_micron=atof(argv[++i]); }
        else if(a=="--connect" && i+1<argc){ opt.connect_stack=split_ws(argv[++i]); }
        else if(a=="--blockages"){ opt.export_as_blockages=true; }
        else if(a=="--map" && i+1<argc){
            if(!custom_map){ opt.layer_map.clear(); custom_map=true; }
            // NAME=L:D[:TYPE]
            std::string s=argv[++i];
            auto eq=s.find('='); if(eq==std::string::npos) { fprintf(stderr,"bad --map: %s\n",s.c_str()); return 2; }
            LayerMapEntry e; e.name=s.substr(0,eq);
            std::string rest=s.substr(eq+1);
            auto c1=rest.find(':');
            if(c1==std::string::npos){ fprintf(stderr,"bad --map: %s\n",s.c_str()); return 2; }
            e.gds_layer=atoi(rest.substr(0,c1).c_str());
            auto c2=rest.find(':',c1+1);
            if(c2==std::string::npos){ e.gds_datatype=atoi(rest.substr(c1+1).c_str()); e.type="ROUTING"; }
            else { e.gds_datatype=atoi(rest.substr(c1+1,c2-c1-1).c_str()); e.type=rest.substr(c2+1); }
            if(e.type!="CUT" && e.type!="ROUTING") e.type="ROUTING";
            opt.layer_map.push_back(e);
        } else if(a=="-h"||a=="--help"){ usage(argv[0]); return 0; }
        else { fprintf(stderr,"unknown arg: %s\n",a.c_str()); usage(argv[0]); return 2; }
    }

    char err[512]={0};
    ldb_file_t* f = ldb_open(in.c_str(), err, sizeof(err));
    if(!f){ fprintf(stderr,"open failed: %s\n",err); return 1; }
    std::string e2;
    int rc = export_lefdef(f, lef, def, opt, e2);
    if(rc!=0){ fprintf(stderr,"export failed: %s\n", e2.c_str()); ldb_close(f); return 1; }
    ldb_extent_t ext{0,0,0,0,0}; ldb_extent_whole(f,&ext);
    printf("OASIS2LEFDEF ok: %s -> %s , %s\n", in.c_str(), lef.c_str(), def.c_str());
    if(ext.valid) printf("DIEAREA %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n",(int64_t)ext.xmin,(int64_t)ext.ymin,(int64_t)ext.xmax,(int64_t)ext.ymax);
    printf("CONNECT:");
    for(auto& s: opt.connect_stack) printf(" %s", s.c_str());
    printf("\n");
    ldb_close(f);
    return 0;
}
