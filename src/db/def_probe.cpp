// File   : def_probe.cpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: DEF routing -> point probes, for checking extracted connectivity (CTR CONNECT /
//          FASCL NET_ID) against the DEF the layout was generated from (CTR exec defcheck).
//
// Streams a .def or .def.gz (zlib). For every regular and special net it turns the routing
// (+ ROUTED / FIXED / COVER / NOSHIELD, + RECT, + VIA) into probes (layer, x, y) on the routing
// layers of a given stack:
//   * a wire segment ( x1 y1 ) ( x2 y2 )  -> its midpoint, on the segment's layer
//   * a via at ( x y )                    -> ( x y ) on the via's bottom and top metal: from the
//                                            DEF VIAS section (+ LAYERS / + RECT) or, for LEF
//                                            vias, the stated layer and the next routing layer
//   * RECT ( dx1 dy1 dx2 dy2 )            -> the rectangle's centre
// The caller looks up the extracted net under each probe (0 = no polygon there) and calls
// fdef_check, which aggregates per DEF net / per extracted net:
//   open  = a DEF net whose probes land on more than one extracted net
//   short = an extracted net that carries probes of more than one DEF net
//   miss  = a probe with no polygon under it
// and gives every extracted net a label (its DEF net name; "a|b|..." for a short).
#include <zlib.h>
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

namespace {

struct Probe { int64_t net; int32_t layer; int32_t kind; int64_t x, y; };   // kind 0 wire 1 via 2 rect

struct DefData {
    std::vector<std::string> net_names;       // 1-based in the API
    std::vector<uint8_t> net_special;
    std::vector<Probe> probes;
    std::map<std::string, int64_t> skipped_layers;   // layer name -> probes not on the stack
    int32_t units = 0;
    int64_t n_polygon_skipped = 0;
    int64_t n_via_defined = 0, n_via_rule = 0, n_lef_vias = 0;   // vias placed: by DEF/LEF definition, by stack rule
    // after fdef_check
    std::vector<std::string> labels;          // per extracted net id (index 0 unused)
    std::string report;
};

std::vector<std::unique_ptr<DefData>> g_defs;
std::string g_err;

bool is_number(const std::string& s) {
    if (s.empty()) return false;
    size_t i = (s[0] == '-' || s[0] == '+') ? 1 : 0;
    if (i >= s.size()) return false;
    bool digit = false;
    for (; i < s.size(); ++i) {
        if (s[i] >= '0' && s[i] <= '9') digit = true;
        else if (s[i] != '.') return false;
    }
    return digit;
}

// Whitespace tokenizer over a zlib stream (plain files read through gzread as well).
class Tokens {
public:
    explicit Tokens(gzFile f) : f_(f) {}
    bool next(std::string& t) {
        if (have_peek_) { t.swap(peek_); have_peek_ = false; return true; }
        return raw(t);
    }
    bool peek(std::string& t) {
        if (!have_peek_) { if (!raw(peek_)) return false; have_peek_ = true; }
        t = peek_;
        return true;
    }
private:
    bool fill() {
        int n = gzread(f_, buf_, sizeof(buf_));
        if (n <= 0) return false;
        pos_ = 0; len_ = n;
        return true;
    }
    bool raw(std::string& t) {
        t.clear();
        for (;;) {                                  // skip blanks and # comments
            if (pos_ >= len_ && !fill()) return false;
            char c = buf_[pos_];
            if (c == '#' && at_line_start_) { comment_ = true; }
            if (comment_) { if (c == '\n') { comment_ = false; at_line_start_ = true; } ++pos_; continue; }
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') { at_line_start_ = (c == '\n') || at_line_start_; ++pos_; continue; }
            break;
        }
        at_line_start_ = false;
        for (;;) {
            if (pos_ >= len_ && !fill()) return !t.empty();
            char c = buf_[pos_];
            if (c == ' ' || c == '\t' || c == '\r' || c == '\n') return true;
            t.push_back(c);
            ++pos_;
        }
    }
    gzFile f_;
    char buf_[1 << 16];
    int pos_ = 0, len_ = 0;
    bool comment_ = false, at_line_start_ = true;
    std::string peek_;
    bool have_peek_ = false;
};

struct Parser {
    Tokens& tk;
    DefData& d;
    std::unordered_map<std::string, int32_t> layer_idx;   // routing layer name -> 1..n
    int32_t nlayers = 0;
    std::unordered_map<std::string, std::pair<int32_t, int32_t>> vias;  // DEF via -> bottom/top idx
    std::string t;

    int32_t lidx(const std::string& name) {
        auto it = layer_idx.find(name);
        return it == layer_idx.end() ? 0 : it->second;
    }
    void add(int64_t net, const std::string& lname, int32_t li, int32_t kind, int64_t x, int64_t y) {
        if (li <= 0) { d.skipped_layers[lname]++; return; }
        d.probes.push_back(Probe{net, li, kind, x, y});
    }
    void add_via(int64_t net, const std::string& vname, const std::string& lname, int64_t x, int64_t y) {
        auto it = vias.find(vname);
        if (it != vias.end() && (it->second.first > 0 || it->second.second > 0)) {
            d.n_via_defined++;
            if (it->second.first > 0) d.probes.push_back(Probe{net, it->second.first, 1, x, y});
            if (it->second.second > 0 && it->second.second != it->second.first)
                d.probes.push_back(Probe{net, it->second.second, 1, x, y});
            return;
        }
        int32_t li = lidx(lname);
        if (li <= 0) { d.skipped_layers[lname]++; return; }
        d.n_via_rule++;
        d.probes.push_back(Probe{net, li, 1, x, y});
        if (li < nlayers) d.probes.push_back(Probe{net, li + 1, 1, x, y});
    }
    void skip_statement() { while (t != ";" && tk.next(t)) {} }

    // LEF: fixed vias `VIA name [DEFAULT|GENERATED] ... LAYER l ; RECT ... END name` give the
    // bottom/top routing layer of LEF vias used in the DEF (VIARULE ... GENERATE blocks are not
    // vias; `VIA name ;` inside a VIARULE is a reference).
    void parse_lef(Tokens& lt) {
        std::string w;
        while (lt.next(w)) {
            if (w != "VIA") continue;
            std::string name, q;
            if (!lt.next(name)) return;
            if (!lt.peek(q) || q == ";") continue;
            int32_t lo = 0, hi = 0;
            while (lt.next(w)) {
                if (w == "END") { lt.next(w); if (w == name) break; continue; }
                if (w == "LAYER") {
                    std::string ln;
                    lt.next(ln);
                    int32_t i = lidx(ln);
                    if (i > 0) { if (lo == 0 || i < lo) lo = i; if (hi == 0 || i > hi) hi = i; }
                }
            }
            if (lo > 0) { vias[name] = {lo, hi}; d.n_lef_vias++; }
        }
    }

    // `- name ... + LAYERS bot cut top | + RECT layer ( ) ( ) ... ;`
    void parse_vias() {
        while (tk.next(t)) {
            if (t == "END") { tk.next(t); return; }
            if (t != "-") continue;
            std::string name;
            tk.next(name);
            int32_t lo = 0, hi = 0;
            auto see = [&](const std::string& ln) {
                int32_t i = lidx(ln);
                if (i <= 0) return;
                if (lo == 0 || i < lo) lo = i;
                if (hi == 0 || i > hi) hi = i;
            };
            while (tk.next(t) && t != ";") {
                if (t == "LAYERS") {
                    std::string a, b, c;
                    tk.next(a); tk.next(b); tk.next(c);
                    see(a); see(c);
                } else if (t == "RECT" || t == "POLYGON") {
                    std::string ln;
                    tk.next(ln);
                    see(ln);
                }
            }
            vias[name] = {lo, hi};
        }
    }

    // Read "( x y [ext] )" after '(' was consumed; '*' keeps the previous coordinate.
    bool point(int64_t& x, int64_t& y) {
        std::string a, b;
        if (!tk.next(a) || !tk.next(b)) return false;
        if (a != "*") x = std::llround(std::atof(a.c_str()));
        if (b != "*") y = std::llround(std::atof(b.c_str()));
        while (tk.next(t) && t != ")") {}
        return true;
    }

    // routing statements after "+ ROUTED|FIXED|COVER|NOSHIELD"; stops at ';' (end of net,
    // left in t) or at a '+' that starts a non-routing net attribute (left in t).
    void parse_routing(int64_t net, bool special) {
        std::string layer;
        int64_t x = 0, y = 0;
        bool havept = false, virt = false;
        auto start = [&]() {
            tk.next(layer);
            havept = false; virt = false;
            if (special) {
                std::string p;
                if (tk.peek(p) && is_number(p)) tk.next(p);   // width
            }
        };
        start();
        while (tk.next(t)) {
            if (t == ";") return;
            if (t == "NEW") { start(); continue; }
            if (t == "+") {
                std::string p;
                if (!tk.peek(p)) return;
                if (p == "SHAPE" || p == "MASK" || p == "STYLE") { tk.next(p); tk.next(p); continue; }
                if (p == "ROUTED" || p == "FIXED" || p == "COVER" || p == "NOSHIELD") { tk.next(p); start(); continue; }
                return;                                       // t == "+": caller handles it
            }
            if (t == "(") {
                int64_t px = x, py = y;
                if (!point(x, y)) return;
                if (havept && !virt && (px != x || py != y))
                    add(net, layer, lidx(layer), 0, (px + x) / 2, (py + y) / 2);
                havept = true; virt = false;
                continue;
            }
            if (t == "VIRTUAL") {
                tk.next(t);                                   // (
                point(x, y);
                havept = true; virt = true;
                continue;
            }
            if (t == "RECT") {
                std::string a, b, c, e;
                tk.next(t); tk.next(a); tk.next(b); tk.next(c); tk.next(e); tk.next(t);
                int64_t cx = x + (std::llround(std::atof(a.c_str())) + std::llround(std::atof(c.c_str()))) / 2;
                int64_t cy = y + (std::llround(std::atof(b.c_str())) + std::llround(std::atof(e.c_str()))) / 2;
                add(net, layer, lidx(layer), 2, cx, cy);
                continue;
            }
            if (t == "MASK" || t == "STYLE" || t == "TAPERRULE" || t == "SHAPE") { tk.next(t); continue; }
            if (t == "TAPER") continue;
            if (t == "DO") {                                  // DO n BY m STEP dx dy (via array)
                for (int i = 0; i < 6; ++i) tk.next(t);
                continue;
            }
            if (is_number(t)) continue;
            static const char* orient[] = {"N", "S", "E", "W", "FN", "FS", "FE", "FW"};
            bool is_orient = false;
            for (const char* o : orient) if (t == o) is_orient = true;
            if (is_orient) continue;
            add_via(net, t, layer, x, y);                     // via at the current point
        }
    }

    void parse_nets(bool special) {
        int64_t net = 0;
        while (tk.next(t)) {
            if (t == "END") { tk.next(t); return; }
            if (t == "-") {
                std::string name, plain;
                tk.next(name);
                // DEF escapes special characters with '\' (OpenROAD: bus\[3\]); the net is bus[3]
                for (size_t i = 0; i < name.size(); ++i) {
                    if (name[i] == '\\' && i + 1 < name.size()) ++i;
                    plain.push_back(name[i]);
                }
                d.net_names.push_back(plain);
                d.net_special.push_back(special ? 1 : 0);
                net = (int64_t)d.net_names.size();
                continue;
            }
            if (net == 0) continue;
            while (t == "+") {
                std::string kw;
                if (!tk.next(kw)) return;
                if (kw == "ROUTED" || kw == "FIXED" || kw == "COVER" || kw == "NOSHIELD") {
                    parse_routing(net, special);          // leaves t = ";" or "+"
                    if (t == ";") break;
                } else if (kw == "RECT" && special) {
                    std::string ln; tk.next(ln);
                    int64_t x1 = 0, y1 = 0, x2 = 0, y2 = 0;
                    tk.next(t); point(x1, y1); x2 = x1; y2 = y1;
                    tk.next(t); point(x2, y2);
                    add(net, ln, lidx(ln), 2, (x1 + x2) / 2, (y1 + y2) / 2);
                    tk.next(t);
                } else if (kw == "POLYGON" && special) {
                    d.n_polygon_skipped++;
                    while (tk.next(t) && t != "+" && t != ";") {}
                } else if (kw == "VIA" && special) {      // + VIA name [orient] ( x y ) ...
                    std::string vn; tk.next(vn);
                    while (tk.next(t) && t != "+" && t != ";") {
                        if (t == "(") {
                            int64_t x = 0, y = 0;
                            point(x, y);
                            add_via(net, vn, "", x, y);
                        }
                    }
                } else {
                    tk.next(t);
                }
                if (t == ";") break;
            }
        }
    }

    bool run() {
        while (tk.next(t)) {
            if (t == "UNITS") {
                std::string a, b, c;
                tk.next(a); tk.next(b); tk.next(c);
                d.units = std::atoi(c.c_str());
                skip_statement();
            } else if (t == "VIAS") {
                skip_statement();
                parse_vias();
            } else if (t == "NETS") {
                skip_statement();
                parse_nets(false);
            } else if (t == "SPECIALNETS") {
                skip_statement();
                parse_nets(true);
            } else if (t == "END") {
                std::string s;
                tk.next(s);
                if (s == "DESIGN") break;
            }
        }
        return true;
    }
};

void copy_str(const std::string& s, char* buf, int32_t n) {
    if (n <= 0) return;
    size_t k = std::min<size_t>(s.size(), (size_t)n - 1);
    std::memcpy(buf, s.data(), k);
    buf[k] = 0;
}

}  // namespace

extern "C" {

// Load a DEF; `layers` lists the stack's routing layer names (DEF spelling) bottom-up,
// separated by blanks. Returns a handle > 0, or 0 on error (fdef_error).
int32_t fdef_open(const char* path, const char* layers, const char* lefs, int64_t* nprobe, int64_t* nnet,
                  int64_t* nskip, int32_t* units) {
    gzFile f = gzopen(path, "rb");
    if (!f) { g_err = std::string("cannot open DEF ") + path; return 0; }
    gzbuffer(f, 1 << 20);
    auto d = std::make_unique<DefData>();
    {
        Tokens tk(f);
        Parser p{tk, *d, {}, 0, {}, {}};
        std::string all(layers), w;
        for (size_t i = 0; i <= all.size(); ++i) {
            if (i == all.size() || all[i] == ' ') {
                if (!w.empty()) p.layer_idx[w] = ++p.nlayers;
                w.clear();
            } else w.push_back(all[i]);
        }
        // LEF via definitions first (DEF VIAS entries override them)
        std::string ls(lefs ? lefs : ""), one;
        for (size_t i = 0; i <= ls.size(); ++i) {
            if (i == ls.size() || ls[i] == ',') {
                if (!one.empty()) {
                    gzFile lf = gzopen(one.c_str(), "rb");
                    if (!lf) { g_err = "cannot open LEF " + one; gzclose(f); return 0; }
                    Tokens lt(lf);
                    p.parse_lef(lt);
                    gzclose(lf);
                }
                one.clear();
            } else one.push_back(ls[i]);
        }
        p.run();
    }
    gzclose(f);
    if (d->net_names.empty()) { g_err = std::string("no NETS / SPECIALNETS in ") + path; return 0; }
    // drop duplicate probes (same net, layer, point)
    auto& P = d->probes;
    std::sort(P.begin(), P.end(), [](const Probe& a, const Probe& b) {
        if (a.net != b.net) return a.net < b.net;
        if (a.layer != b.layer) return a.layer < b.layer;
        if (a.x != b.x) return a.x < b.x;
        return a.y < b.y;
    });
    P.erase(std::unique(P.begin(), P.end(), [](const Probe& a, const Probe& b) {
        return a.net == b.net && a.layer == b.layer && a.x == b.x && a.y == b.y;
    }), P.end());
    *nprobe = (int64_t)P.size();
    *nnet = (int64_t)d->net_names.size();
    int64_t s = 0;
    for (auto& kv : d->skipped_layers) s += kv.second;
    *nskip = s;
    *units = d->units;
    g_defs.push_back(std::move(d));
    return (int32_t)g_defs.size();
}

int32_t fdef_probes(int32_t h, int32_t* layer, int64_t* x, int64_t* y, int64_t* net, int32_t* kind) {
    if (h <= 0 || h > (int32_t)g_defs.size() || !g_defs[h - 1]) return 1;
    auto& P = g_defs[h - 1]->probes;
    for (size_t i = 0; i < P.size(); ++i) {
        layer[i] = P[i].layer; x[i] = P[i].x; y[i] = P[i].y; net[i] = P[i].net; kind[i] = P[i].kind;
    }
    return 0;
}

// Aggregate. nid[i] = extracted net under probe i (0: no polygon). stats (int64[16]):
//  0 probes  1 hit  2 missed  3 DEF nets  4 DEF nets probed  5 DEF nets clean (one extracted net,
//  no miss)  6 opens  7 DEF nets with misses  8 extracted nets named  9 shorts
//  10 extracted nets without a DEF name  11 DEF nets without probes  12 special DEF nets
//  13 probes on layers outside the stack  14 special-net opens  15 special-net shorts
// prefix != "": writes <prefix>_opens.csv, _shorts.csv, _misses.csv, _names.csv
int32_t fdef_check(int32_t h, const int64_t* nid, int64_t n_ext, const char* prefix,
                   const char* layer_names, int64_t* stats) {
    if (h <= 0 || h > (int32_t)g_defs.size() || !g_defs[h - 1]) return 1;
    DefData& d = *g_defs[h - 1];
    auto& P = d.probes;
    const int64_t nd = (int64_t)d.net_names.size();
    std::vector<std::string> lname(1, "?");
    {
        std::string all(layer_names), w;
        for (size_t i = 0; i <= all.size(); ++i) {
            if (i == all.size() || all[i] == ' ') { if (!w.empty()) lname.push_back(w); w.clear(); }
            else w.push_back(all[i]);
        }
    }
    auto ln = [&](int32_t l) { return l > 0 && l < (int32_t)lname.size() ? lname[l] : std::string("?"); };
    for (int i = 0; i < 16; ++i) stats[i] = 0;
    stats[0] = (int64_t)P.size();
    // (def net, ext net, first probe) pairs
    struct Pair { int64_t dn, en; size_t p; };
    std::vector<Pair> pairs;
    pairs.reserve(P.size());
    std::vector<int64_t> miss(nd + 1, 0), nprobe(nd + 1, 0);
    for (size_t i = 0; i < P.size(); ++i) {
        nprobe[P[i].net]++;
        if (nid[i] <= 0 || nid[i] > n_ext) { stats[2]++; miss[P[i].net]++; continue; }
        stats[1]++;
        pairs.push_back(Pair{P[i].net, nid[i], i});
    }
    std::sort(pairs.begin(), pairs.end(), [](const Pair& a, const Pair& b) {
        return a.dn != b.dn ? a.dn < b.dn : (a.en != b.en ? a.en < b.en : a.p < b.p);
    });
    pairs.erase(std::unique(pairs.begin(), pairs.end(), [](const Pair& a, const Pair& b) {
        return a.dn == b.dn && a.en == b.en; }), pairs.end());
    stats[3] = nd;
    for (int64_t k = 1; k <= nd; ++k) {
        if (d.net_special[k - 1]) stats[12]++;
        if (nprobe[k] > 0) stats[4]++; else stats[11]++;
        if (miss[k] > 0) stats[7]++;
    }
    std::string pre(prefix ? prefix : "");
    FILE* fo = pre.empty() ? nullptr : std::fopen((pre + "_opens.csv").c_str(), "w");
    if (fo) std::fprintf(fo, "def_net,special,n_extracted,extracted_net,layer,x,y\n");
    std::string ex_open, ex_short;
    int n_ex_open = 0, n_ex_short = 0;
    for (size_t i = 0; i < pairs.size();) {
        size_t j = i;
        while (j < pairs.size() && pairs[j].dn == pairs[i].dn) ++j;
        int64_t dn = pairs[i].dn;
        if (j - i == 1 && miss[dn] == 0) stats[5]++;
        if (j - i > 1) {
            stats[6]++;
            if (d.net_special[dn - 1]) stats[14]++;
            for (size_t k = i; k < j && fo; ++k) {
                const Probe& q = P[pairs[k].p];
                std::fprintf(fo, "%s,%d,%zu,%lld,%s,%lld,%lld\n", d.net_names[dn - 1].c_str(), (int)d.net_special[dn - 1],
                             j - i, (long long)pairs[k].en, ln(q.layer).c_str(), (long long)q.x, (long long)q.y);
            }
            if (n_ex_open < 5) {
                const Probe& q = P[pairs[i].p];
                const Probe& r = P[pairs[i + 1].p];
                char b[512];
                std::snprintf(b, sizeof b, "  open : DEF net %s on %zu extracted nets, e.g. %s (%lld,%lld) vs %s (%lld,%lld)\n",
                              d.net_names[dn - 1].c_str(), j - i, ln(q.layer).c_str(), (long long)q.x, (long long)q.y,
                              ln(r.layer).c_str(), (long long)r.x, (long long)r.y);
                ex_open += b;
                n_ex_open++;
            }
        }
        i = j;
    }
    if (fo) std::fclose(fo);
    // per extracted net
    std::sort(pairs.begin(), pairs.end(), [](const Pair& a, const Pair& b) {
        return a.en != b.en ? a.en < b.en : a.dn < b.dn; });
    d.labels.assign(n_ext + 1, std::string());
    FILE* fs = pre.empty() ? nullptr : std::fopen((pre + "_shorts.csv").c_str(), "w");
    if (fs) std::fprintf(fs, "extracted_net,n_def,def_net,special,layer,x,y\n");
    int64_t named = 0;
    for (size_t i = 0; i < pairs.size();) {
        size_t j = i;
        while (j < pairs.size() && pairs[j].en == pairs[i].en) ++j;
        int64_t en = pairs[i].en;
        named++;
        if (j - i == 1) {
            stats[8]++;
            d.labels[en] = d.net_names[pairs[i].dn - 1];
        } else {
            stats[9]++;
            bool sp = false;
            std::string lab;
            for (size_t k = i; k < j; ++k) {
                if (d.net_special[pairs[k].dn - 1]) sp = true;
                if (k - i < 8) { if (!lab.empty()) lab += "|"; lab += d.net_names[pairs[k].dn - 1]; }
                if (fs) {
                    const Probe& q = P[pairs[k].p];
                    std::fprintf(fs, "%lld,%zu,%s,%d,%s,%lld,%lld\n", (long long)en, j - i, d.net_names[pairs[k].dn - 1].c_str(),
                                 (int)d.net_special[pairs[k].dn - 1], ln(q.layer).c_str(), (long long)q.x, (long long)q.y);
                }
            }
            if (j - i > 8) lab += "|...";
            if (sp) stats[15]++;
            d.labels[en] = lab;
            if (n_ex_short < 5) {
                const Probe& q = P[pairs[i].p];
                char b[512];
                std::snprintf(b, sizeof b, "  short: extracted net %lld carries %zu DEF nets (%s), e.g. %s (%lld,%lld)\n",
                              (long long)en, j - i, lab.substr(0, 200).c_str(), ln(q.layer).c_str(), (long long)q.x, (long long)q.y);
                ex_short += b;
                n_ex_short++;
            }
        }
        i = j;
    }
    if (fs) std::fclose(fs);
    stats[10] = n_ext - named;
    if (!pre.empty()) {
        FILE* fm = std::fopen((pre + "_misses.csv").c_str(), "w");
        if (fm) {
            std::fprintf(fm, "def_net,special,kind,layer,x,y\n");
            static const char* kn[] = {"wire", "via", "rect"};
            for (size_t i = 0; i < P.size(); ++i)
                if (nid[i] <= 0 || nid[i] > n_ext)
                    std::fprintf(fm, "%s,%d,%s,%s,%lld,%lld\n", d.net_names[P[i].net - 1].c_str(), (int)d.net_special[P[i].net - 1],
                                 kn[P[i].kind], ln(P[i].layer).c_str(), (long long)P[i].x, (long long)P[i].y);
            std::fclose(fm);
        }
        FILE* fn = std::fopen((pre + "_names.csv").c_str(), "w");
        if (fn) {
            std::fprintf(fn, "extracted_net,def_net\n");
            for (int64_t e = 1; e <= n_ext; ++e)
                if (!d.labels[e].empty()) std::fprintf(fn, "%lld,%s\n", (long long)e, d.labels[e].c_str());
            std::fclose(fn);
        }
    }
    for (auto& kv : d.skipped_layers) stats[13] += kv.second;
    d.report = ex_open + ex_short;
    if (!d.skipped_layers.empty()) {
        std::string s = "  probes on layers outside the stack:";
        for (auto& kv : d.skipped_layers) s += " " + (kv.first.empty() ? std::string("?") : kv.first) + "=" + std::to_string(kv.second);
        d.report += s + "\n";
    }
    d.report += "  vias: " + std::to_string(d.n_via_defined) + " placed from DEF/LEF definitions (" +
                std::to_string(d.n_lef_vias) + " LEF vias read), " + std::to_string(d.n_via_rule) +
                " by the stack rule (stated layer + next routing layer)\n";
    if (d.n_polygon_skipped) d.report += "  special-net POLYGONs not probed: " + std::to_string(d.n_polygon_skipped) + "\n";
    return 0;
}

// Label of extracted net e after fdef_check (length; 0 = no DEF net).
int32_t fdef_label(int32_t h, int64_t e, char* buf, int32_t buflen) {
    if (h <= 0 || h > (int32_t)g_defs.size() || !g_defs[h - 1]) return 0;
    auto& L = g_defs[h - 1]->labels;
    if (e <= 0 || e >= (int64_t)L.size()) { copy_str("", buf, buflen); return 0; }
    copy_str(L[e], buf, buflen);
    return (int32_t)std::min<size_t>(L[e].size(), (size_t)buflen - 1);
}

int32_t fdef_report(int32_t h, char* buf, int32_t buflen) {
    if (h <= 0 || h > (int32_t)g_defs.size() || !g_defs[h - 1]) return 0;
    copy_str(g_defs[h - 1]->report, buf, buflen);
    return (int32_t)std::min<size_t>(g_defs[h - 1]->report.size(), (size_t)buflen - 1);
}

void fdef_close(int32_t h) {
    if (h > 0 && h <= (int32_t)g_defs.size()) g_defs[h - 1].reset();
}

int32_t fdef_error(char* buf, int32_t buflen) {
    copy_str(g_err, buf, buflen);
    return (int32_t)g_err.size();
}

// Database units per micron of a GDSII (UNITS record) or OASIS (START unit) file, plain or
// gzip. Returns 0 and sets *dbu, or 1 if the header cannot be read.
int32_t fdef_layout_dbu(const char* path, double* dbu) {
    *dbu = 0;
    gzFile f = gzopen(path, "rb");
    if (!f) return 1;
    unsigned char b[1 << 16];
    int n = gzread(f, b, sizeof b);
    gzclose(f);
    if (n < 16) return 1;
    static const char oas[] = "%SEMI-OASIS\r\n";
    if (std::memcmp(b, oas, 13) == 0) {
        size_t p = 13;
        auto uint = [&](uint64_t& v) {
            v = 0;
            int sh = 0;
            while (p < (size_t)n) {
                unsigned char c = b[p++];
                v |= (uint64_t)(c & 0x7f) << sh;
                if (!(c & 0x80)) return true;
                sh += 7;
            }
            return false;
        };
        uint64_t v;
        if (!uint(v) || v != 1) return 1;                      // START
        if (!uint(v)) return 1;                                // version a-string
        p += v;
        uint64_t type;
        if (!uint(type)) return 1;
        double u = 0;
        uint64_t a = 0, c = 0;
        switch (type) {
        case 0: case 1: if (!uint(a)) return 1; u = (double)a; break;
        case 2: case 3: if (!uint(a) || a == 0) return 1; u = 1.0 / (double)a; break;
        case 4: case 5: if (!uint(a) || !uint(c) || c == 0) return 1; u = (double)a / (double)c; break;
        case 6: { float x; if (p + 4 > (size_t)n) return 1; std::memcpy(&x, b + p, 4); u = x; break; }
        case 7: { double x; if (p + 8 > (size_t)n) return 1; std::memcpy(&x, b + p, 8); u = x; break; }
        default: return 1;
        }
        *dbu = u;
        return u > 0 ? 0 : 1;
    }
    // GDSII: records (len16, type8, dtype8); UNITS = 0x0305, two 8-byte excess-64 reals
    size_t p = 0;
    while (p + 4 <= (size_t)n) {
        size_t len = ((size_t)b[p] << 8) | b[p + 1];
        if (len < 4) return 1;
        if (b[p + 2] == 0x03 && b[p + 3] == 0x05 && p + 20 <= (size_t)n) {
            auto real8 = [&](const unsigned char* r) {
                int sign = r[0] & 0x80 ? -1 : 1;
                int exp = (r[0] & 0x7f) - 64;
                double m = 0;
                for (int i = 1; i < 8; ++i) m = m * 256.0 + r[i];
                m /= std::pow(2.0, 56);
                return sign * m * std::pow(16.0, exp);
            };
            double meters = real8(b + p + 12);                 // database unit in meters
            if (meters <= 0) return 1;
            *dbu = 1e-6 / meters;
            return 0;
        }
        p += len;
    }
    return 1;
}

}  // extern "C"
