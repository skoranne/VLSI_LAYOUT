#include "lefdef_export.h"
#include <cstdio>
#include <fstream>
#include <sstream>
#include <cmath>
#include <algorithm>
#include <cinttypes>
#include <set>
#include <map>
#include <vector>
#include <string>
#include <functional>
#include <limits>
#include <numeric>

std::vector<LayerMapEntry> default_sky130_connect_map() {
    // CONNECT LI1 VIA MET1 VIA1 MET2 VIA2 MET3 VIA3 MET4 VIA4 MET5
    std::vector<LayerMapEntry> m;
    auto add = [&](const char* n,const char* t,int l,int d,double w){
        LayerMapEntry e; e.name=n; e.type=t; e.gds_layer=l; e.gds_datatype=d; e.width=w; m.push_back(e);
    };
    add("LI1","ROUTING",67,20,0.17);
    add("VIA","CUT",    67,44,0.17);
    add("MET1","ROUTING",68,20,0.14);
    add("VIA1","CUT",    68,44,0.15);
    add("MET2","ROUTING",69,20,0.14);
    add("VIA2","CUT",    69,44,0.20);
    add("MET3","ROUTING",70,20,0.30);
    add("VIA3","CUT",    70,44,0.20);
    add("MET4","ROUTING",71,20,0.30);
    add("VIA4","CUT",    71,44,0.80);
    add("MET5","ROUTING",72,20,1.60);
    return m;
}

struct BoxRec { std::string cell; int layer,datatype,kind; int64_t x1,y1,x2,y2; };

static void box_collect_cb(void* ud, const char* cell, int layer, int datatype, int element_kind,
                           int64_t x1, int64_t y1, int64_t x2, int64_t y2){
    auto* v = (std::vector<BoxRec>*)ud;
    v->push_back({cell?cell:"", layer, datatype, element_kind, x1,y1,x2,y2});
}

static std::string qname(const std::string& s){
    for(char c: s){ if(!isalnum((unsigned char)c) && c!='_' && c!='[' && c!=']' && c!='/' && c!='-') return "\"" + s + "\""; }
    return s.empty() ? "NONAME" : s;
}

static inline bool boxes_touch(const BoxRec& a, const BoxRec& b){
    // touch or overlap (inclusive edges) for connectivity
    if(a.x2 < b.x1 || b.x2 < a.x1) return false;
    if(a.y2 < b.y1 || b.y2 < a.y1) return false;
    return true;
}
static inline bool boxes_overlap(const BoxRec& a, const BoxRec& b){
    if(a.x2 <= b.x1 || b.x2 <= a.x1) return false;
    if(a.y2 <= b.y1 || b.y2 <= a.y1) return false;
    return true;
}

struct DSU {
    std::vector<int> p, r;
    DSU(int n=0){ init(n); }
    void init(int n){ p.resize(n); r.assign(n,0); std::iota(p.begin(),p.end(),0); }
    int f(int x){ return p[x]==x?x:p[x]=f(p[x]); }
    void u(int a,int b){ a=f(a); b=f(b); if(a==b) return; if(r[a]<r[b]) std::swap(a,b); p[b]=a; if(r[a]==r[b]) r[a]++; }
};

int export_lefdef(ldb_file_t* f, const std::string& lef_path, const std::string& def_path,
                  const LefDefOptions& opt, std::string& err){
    if(!f){ err="null file"; return -1; }
    LefDefOptions o = opt;
    if(o.layer_map.empty()) o.layer_map = default_sky130_connect_map();
    if(o.connect_stack.empty()){
        o.connect_stack = {"LI1","VIA","MET1","VIA1","MET2","VIA2","MET3","VIA3","MET4","VIA4","MET5"};
    }
    std::map<std::pair<int,int>, const LayerMapEntry*> by_gds;
    std::map<std::string, const LayerMapEntry*> by_name;
    for(auto& e: o.layer_map){
        by_name[e.name] = &e;
        if(e.gds_layer>=0) by_gds[{e.gds_layer,e.gds_datatype}] = &e;
    }

    std::vector<BoxRec> boxes;
    ldb_iterate_boxes(f, box_collect_cb, &boxes);

    ldb_extent_t ext{0,0,0,0,0};
    ldb_extent_whole(f,&ext);
    if(!ext.valid && !boxes.empty()){
        ext.valid=1; ext.xmin=ext.ymin=std::numeric_limits<int64_t>::max()/4; ext.xmax=ext.ymax=std::numeric_limits<int64_t>::min()/4;
        for(auto& b: boxes){ ext.xmin=std::min(ext.xmin,b.x1); ext.ymin=std::min(ext.ymin,b.y1); ext.xmax=std::max(ext.xmax,b.x2); ext.ymax=std::max(ext.ymax,b.y2);}    }

    // Build per-mapped-layer shape lists (indices into local vectors)
    struct Shape { int gidx; const BoxRec* b; int net; };
    std::map<std::string, std::vector<int>> layer_to_gidx; // layer name -> indices in boxes
    std::map<std::string, std::vector<int>> routing_shapes; // layer name -> shape ids in 'shapes'
    std::map<std::string, std::vector<int>> cut_shapes;
    std::vector<Shape> shapes;
    for(size_t i=0;i<boxes.size();++i){
        auto it = by_gds.find({boxes[i].layer, boxes[i].datatype});
        if(it==by_gds.end()) continue;
        layer_to_gidx[it->second->name].push_back((int)i);
        Shape s; s.gidx=(int)i; s.b=&boxes[i]; s.net=-1;
        int sid=(int)shapes.size(); shapes.push_back(s);
        if(it->second->type=="ROUTING") routing_shapes[it->second->name].push_back(sid);
        else cut_shapes[it->second->name].push_back(sid);
    }

    // Connectivity extraction (net-by-net):
    // 1) Union routing shapes on same layer that touch/overlap.
    // 2) For each CUT layer in CONNECT stack, if a cut overlaps/touches a routing shape on layer below and above, union them.
    DSU dsu((int)shapes.size());
    auto connect_layer_intralayer = [&](const std::string& lname){
        auto it = routing_shapes.find(lname); if(it==routing_shapes.end()) return;
        auto& ids = it->second;
        // naive O(n^2); acceptable for moderate layouts, can be improved with sweep
        for(size_t i=0;i<ids.size();++i){
            for(size_t j=i+1;j<ids.size();++j){
                if(boxes_touch(*shapes[ids[i]].b, *shapes[ids[j]].b)) dsu.u(ids[i], ids[j]);
            }
        }
    };
    for(auto& kv: routing_shapes) connect_layer_intralayer(kv.first);

    // cut connectivity and via enclosure records
    struct ViaInst { std::string vname; std::string cut_layer; std::string bot; std::string top; int64_t x,y; int cut_sid; int net; };
    std::vector<ViaInst> vias;
    for(size_t i=0;i+2<o.connect_stack.size(); ++i){
        std::string nbot=o.connect_stack[i], ncut=o.connect_stack[i+1], ntop=o.connect_stack[i+2];
        auto ib=by_name.find(nbot), ic=by_name.find(ncut), itp=by_name.find(ntop);
        if(ib==by_name.end()||ic==by_name.end()||itp==by_name.end()) continue;
        if(ib->second->type!="ROUTING" || ic->second->type!="CUT" || itp->second->type!="ROUTING") continue;
        auto cit = cut_shapes.find(ncut); if(cit==cut_shapes.end()) continue;
        auto bit = routing_shapes.find(nbot);
        auto tit = routing_shapes.find(ntop);
        for(int csid : cit->second){
            const BoxRec& cbox = *shapes[csid].b;
            int bconn=-1, tconn=-1;
            if(bit!=routing_shapes.end()){
                for(int bsid: bit->second){ if(boxes_touch(cbox,*shapes[bsid].b)){ bconn=bsid; break; } }
            }
            if(tit!=routing_shapes.end()){
                for(int tsid: tit->second){ if(boxes_touch(cbox,*shapes[tsid].b)){ tconn=tsid; break; } }
            }
            if(bconn>=0 && tconn>=0){
                dsu.u(bconn,tconn);
                dsu.u(bconn,csid);
                // enclosure/via at cut center
                ViaInst vi; vi.vname = "VIA_" + nbot + "_" + ntop;
                vi.cut_layer=ncut; vi.bot=nbot; vi.top=ntop;
                vi.x=(cbox.x1+cbox.x2)/2; vi.y=(cbox.y1+cbox.y2)/2; vi.cut_sid=csid; vi.net=-1;
                vias.push_back(vi);
            } else if(bconn>=0){ dsu.u(bconn,csid); }
            else if(tconn>=0){ dsu.u(tconn,csid); }
        }
    }

    // assign net ids
    std::map<int,int> root_to_net;
    int netcount=0;
    for(size_t i=0;i<shapes.size();++i){
        int r=dsu.f((int)i);
        auto it=root_to_net.find(r);
        if(it==root_to_net.end()){ root_to_net[r]=netcount++; }
        shapes[i].net=root_to_net[r];
    }
    for(auto& v: vias){ v.net = shapes[v.cut_sid].net; }

    // gather shapes by net and by (net,layer)
    struct NetInfo { int id; std::map<std::string,std::vector<int>> by_layer; std::vector<int> vi; };
    std::map<int, NetInfo> nets;
    for(auto& kv: root_to_net){ nets[kv.second].id=kv.second; }
    for(size_t i=0;i<shapes.size();++i){
        auto itg = by_gds.find({shapes[i].b->layer, shapes[i].b->datatype});
        if(itg==by_gds.end()) continue;
        nets[shapes[i].net].by_layer[itg->second->name].push_back((int)i);
    }
    for(size_t i=0;i<vias.size();++i){ if(vias[i].net>=0) nets[vias[i].net].vi.push_back((int)i); }

    // write LEF
    {
        std::ofstream lf(lef_path.c_str());
        if(!lf){ err="cannot open LEF for write: "+lef_path; return -1; }
        lf << "VERSION 5.8 ;\nBUSBITCHARS \"[]\" ;\nDIVIDERCHAR \"/\" ;\n\n";
        lf << "UNITS\n  DATABASE MICRONS " << (long long)o.dbu_per_micron << " ;\nEND UNITS\n\n";
        std::set<std::string> emitted;
        auto emit_layer = [&](const LayerMapEntry& e){
            if(emitted.count(e.name)) return; emitted.insert(e.name);
            lf << "LAYER " << e.name << "\n";
            if(e.type=="ROUTING"){
                lf << "  TYPE ROUTING ;\n  DIRECTION HORIZONTAL ;\n";
                if(e.width>0) lf << "  WIDTH " << e.width << " ;\n";
            } else {
                lf << "  TYPE CUT ;\n";
            }
            lf << "END " << e.name << "\n\n";
        };
        for(auto& nm: o.connect_stack){ auto it=by_name.find(nm); if(it!=by_name.end()) emit_layer(*it->second); }
        for(auto& e: o.layer_map){ emit_layer(e); }
        // standard via defs
        for(size_t i=0;i+2<o.connect_stack.size(); i++){
            auto n0=o.connect_stack[i], n1=o.connect_stack[i+1], n2=o.connect_stack[i+2];
            auto i0=by_name.find(n0), i1=by_name.find(n1), i2=by_name.find(n2);
            if(i0==by_name.end()||i1==by_name.end()||i2==by_name.end()) continue;
            if(i0->second->type=="ROUTING" && i1->second->type=="CUT" && i2->second->type=="ROUTING"){
                std::string vname = "VIA_" + n0 + "_" + n2;
                lf << "VIA " << vname << " DEFAULT\n";
                lf << "  LAYER " << n0 << " ;\n    RECT -0.085 -0.085 0.085 0.085 ;\n";
                lf << "  LAYER " << n1 << " ;\n    RECT -0.085 -0.085 0.085 0.085 ;\n";
                lf << "  LAYER " << n2 << " ;\n    RECT -0.085 -0.085 0.085 0.085 ;\n";
                lf << "END " << vname << "\n\n";
            }
        }
        lf << "END LIBRARY\n";
    }

    // write DEF
    {
        std::ofstream df(def_path.c_str());
        if(!df){ err="cannot open DEF for write: "+def_path; return -1; }
        df << "VERSION 5.8 ;\nDIVIDERCHAR \"/\" ;\nBUSBITCHARS \"[]\" ;\n";
        df << "DESIGN " << qname(o.design_name) << " ;\n";
        df << "UNITS DISTANCE MICRONS " << (long long)o.dbu_per_micron << " ;\n\n";
        if(ext.valid) df << "DIEAREA ( " << ext.xmin << " " << ext.ymin << " ) ( " << ext.xmax << " " << ext.ymax << " ) ;\n\n";
        else df << "DIEAREA ( 0 0 ) ( 0 0 ) ;\n\n";

        // Components/Pins minimal
        df << "COMPONENTS 0 ;\nEND COMPONENTS\n\nPINS 0 ;\nEND PINS\n\n";

        // NETS with per-net connectivity, routed shapes, and VIA instances for each enclosure
        if(!nets.empty()){
            df << "NETS " << nets.size() << " ;\n";
            for(auto& nkv: nets){
                auto& net = nkv.second;
                df << "  - NET_" << net.id << "\n";
                // routing shapes
                bool first = true;
                for(auto& nm : o.connect_stack){
                    auto lit = net.by_layer.find(nm);
                    if(lit==net.by_layer.end()) continue;
                    auto eit = by_name.find(nm);
                    if(eit==by_name.end() || eit->second->type!="ROUTING") continue;
                    for(int sid: lit->second){
                        const BoxRec& b = *shapes[sid].b;
                        int64_t cx=(b.x1+b.x2)/2, cy=(b.y1+b.y2)/2;
                        df << "    " << (first?"  + ROUTED ":"    NEW ") << nm << " 0 ( " << cx << " " << cy << " )\n";
                        df << "      RECT ( " << (b.x1-cx) << " " << (b.y1-cy) << " " << (b.x2-cx) << " " << (b.y2-cy) << " )\n";
                        first=false;
                    }
                }
                // VIA for each enclosure (cut shape bridging layers)
                for(int vi: net.vi){
                    auto& v = vias[vi];
                    df << "    " << (first?"  + ROUTED ":"    NEW ") << v.bot << " ( " << v.x << " " << v.y << " ) " << v.vname << "\n";
                    first=false;
                }
                if(first){ df << "    + USE SIGNAL ;\n"; }
                else df << "    + USE SIGNAL ;\n";
            }
            df << "END NETS\n\n";
        } else {
            df << "NETS 0 ;\nEND NETS\n\n";
        }

        df << "END DESIGN\n";
    }

    return 0;
}
