// File   : def_reader.hpp
// Author : Sandeep Koranne (C) 2026. All rights reserved.
// Purpose: Streaming DEF (.def or .def.gz through zlib) and LEF reader shared by db_defcheck
//          (layout vs DEF cross-check) and db_def2oas (DEF -> OASIS with names and properties).
//          Header-only; everything is in namespace defr.
#pragma once
#include <zlib.h>
#include <algorithm>
#include <array>
#include <cctype>
#include <cinttypes>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace defr {

// ------------------------------------------------------------------ tokenizer (plain or gz)
class Tokenizer {
public:
    bool open(const std::string& path) {
        g_ = gzopen(path.c_str(), "rb");
        if (!g_) return false;
        gzbuffer(g_, 1 << 20);
        buf_.resize(1 << 20);
        return true;
    }
    ~Tokenizer() { if (g_) gzclose(g_); }
    // Next whitespace-separated token; '(' ')' ';' are always tokens of their own,
    // "#" starts a comment to end of line, "..." is one token (quotes kept).
    bool next(std::string& t) {
        if (!pending_.empty()) { t.swap(pending_.back()); pending_.pop_back(); return true; }
        t.clear();
        int c;
        for (;;) {
            c = get();
            if (c < 0) return false;
            if (c == '#') { while ((c = get()) >= 0 && c != '\n') {} continue; }
            if (c == '\n') ++line_;
            if (!isspace(c)) break;
        }
        if (c == '(' || c == ')' || c == ';') { t.push_back((char)c); return true; }
        if (c == '"') {
            t.push_back('"');
            while ((c = get()) >= 0) { if (c == '\\') { int d = get(); if (d >= 0) t.push_back((char)d); continue; }
                t.push_back((char)c); if (c == '"') break; if (c == '\n') ++line_; }
            return true;
        }
        do {
            if (c == '(' || c == ')' || c == ';') { unget(); break; }
            t.push_back((char)c);
            c = get();
        } while (c >= 0 && !isspace(c));
        if (c == '\n') ++line_;
        return true;
    }
    bool peek(std::string& t) {
        if (pending_.empty()) { std::string u; if (!next(u)) return false; pending_.push_back(u); }
        t = pending_.back(); return true;
    }
    // push a token back; tokens come back in LIFO order
    void push_back(const std::string& t) { pending_.push_back(t); }
    // skip through the next ';'
    void skip_statement() { std::string t; while (next(t)) if (t == ";") return; }
    int64_t line() const { return line_; }
    int64_t bytes_in() const { return g_ ? (int64_t)gzoffset(g_) : 0; }
private:
    int get() {
        if (pos_ >= len_) {
            if (eof_) return -1;
            int n = gzread(g_, buf_.data(), (unsigned)buf_.size());
            if (n <= 0) { eof_ = true; return -1; }
            len_ = (size_t)n; pos_ = 0;
        }
        return (unsigned char)buf_[pos_++];
    }
    void unget() { if (pos_ > 0) --pos_; }
    gzFile g_ = nullptr;
    std::vector<char> buf_;
    size_t pos_ = 0, len_ = 0;
    bool eof_ = false;
    std::vector<std::string> pending_;
    int64_t line_ = 1;
};

bool is_num(const std::string& s) {
    if (s.empty()) return false;
    char* e = nullptr;
    strtod(s.c_str(), &e);
    return e && *e == 0;
}

// ------------------------------------------------------------------ geometry helpers
struct Rect { int64_t x1, y1, x2, y2; };

// DEF/LEF orientation -> (angle, mirror_x) as in GDS/OASIS (mirror about x first, then rotate)
int orient_code(const std::string& o) {   // 0..7: N W S E FS FW FN FE
    static const char* names[8] = {"N", "W", "S", "E", "FS", "FW", "FN", "FE"};
    for (int i = 0; i < 8; ++i) if (o == names[i]) return i;
    return -1;
}
const char* orient_name(int c) {
    static const char* names[8] = {"N", "W", "S", "E", "FS", "FW", "FN", "FE"};
    return (c >= 0 && c < 8) ? names[c] : "?";
}
void orient_apply(int code, int64_t x, int64_t y, int64_t& ox, int64_t& oy) {
    if (code >= 4) y = -y;                       // mirror about x axis
    switch (code & 3) {
        case 0: ox = x;  oy = y;  break;          // 0
        case 1: ox = -y; oy = x;  break;          // 90
        case 2: ox = -x; oy = -y; break;          // 180
        default: ox = y; oy = -x; break;          // 270
    }
}
Rect orient_rect(int code, const Rect& r, int64_t dx, int64_t dy) {
    int64_t ax, ay, bx, by;
    orient_apply(code, r.x1, r.y1, ax, ay);
    orient_apply(code, r.x2, r.y2, bx, by);
    return {std::min(ax, bx) + dx, std::min(ay, by) + dy, std::max(ax, bx) + dx, std::max(ay, by) + dy};
}

// ------------------------------------------------------------------ layer stack
struct StackLayer {
    std::string name;
    bool cut = false;
    int dl = -1, dd = -1;   // drawing
    int pl = -1, pd = -1;   // pin datatype (optional)
    double lef_width_um = 0;
};

bool parse_ld(const std::string& s, int& l, int& d) {
    return sscanf(s.c_str(), "%d/%d", &l, &d) == 2;
}

bool parse_stack(const std::string& spec, std::vector<StackLayer>& out) {
    out.clear();
    size_t i = 0;
    while (i < spec.size()) {
        while (i < spec.size() && (isspace((unsigned char)spec[i]) || spec[i] == ',')) ++i;
        if (i >= spec.size()) break;
        size_t j = i;
        while (j < spec.size() && !isspace((unsigned char)spec[j]) && spec[j] != ',') ++j;
        std::string item = spec.substr(i, j - i);
        i = j;
        StackLayer L;
        size_t c1 = item.find(':');
        if (c1 == std::string::npos) return false;
        L.name = item.substr(0, c1);
        std::string rest = item.substr(c1 + 1);
        size_t c2 = rest.find(':');
        if (!parse_ld(rest.substr(0, c2), L.dl, L.dd)) return false;
        if (c2 != std::string::npos && !parse_ld(rest.substr(c2 + 1), L.pl, L.pd)) return false;
        L.cut = (out.size() % 2) == 1;
        out.push_back(L);
    }
    return !out.empty();
}

const char* kSky130Stack =
    "li1:67/20:67/16 mcon:67/44 met1:68/20:68/16 via:68/44 met2:69/20:69/16 via2:69/44 "
    "met3:70/20:70/16 via3:70/44 met4:71/20:71/16 via4:71/44 met5:72/20:72/16";

// ------------------------------------------------------------------ LEF model
struct ViaDef {
    std::vector<std::pair<std::string, Rect>> rects;   // layer name, rect (DEF/layout dbu)
    bool cuts_exact = true;
    bool has_polygon = false;   // a + POLYGON shape: probed by nothing, XOR layer incomplete
};
struct LefRect { std::string layer; double x1, y1, x2, y2; bool pin; };
struct LefMacro {
    double w = 0, h = 0, ox = 0, oy = 0;
    bool has_size = false;
    std::vector<LefRect> rects;
    int64_t n_polygons = 0;
};
struct LefVia { std::vector<std::pair<std::string, std::array<double, 4>>> rects; };

struct Lef {
    std::map<std::string, LefMacro> macros;
    std::map<std::string, LefVia> vias;
    std::map<std::string, double> layer_width;   // routing width in microns
    std::map<std::string, std::string> layer_type;
};

bool read_rect4(Tokenizer& tk, double v[4]) {
    std::string t;
    int n = 0;
    while (n < 4 && tk.next(t)) {
        if (t == "MASK") { tk.next(t); continue; }
        if (t == "ITERATE") return false;
        if (!is_num(t)) return false;
        v[n++] = atof(t.c_str());
    }
    if (n < 4) return false;
    while (tk.next(t) && t != ";") {}
    return true;
}

void skip_to_end(Tokenizer& tk, const std::string& name) {
    std::string t, u;
    while (tk.next(t)) {
        if (t == "END") { if (!tk.next(u)) return; if (u == name) return; }
    }
}

bool parse_lef(const std::string& path, Lef& lef, std::string& err) {
    Tokenizer tk;
    if (!tk.open(path)) { err = "cannot open LEF " + path; return false; }
    std::string t, u;
    while (tk.next(t)) {
        if (t == "LAYER") {
            std::string name; tk.next(name);
            std::string type;
            while (tk.next(u)) {
                if (u == "END") { tk.next(u); if (u == name) break; continue; }
                if (u == "TYPE") { tk.next(type); lef.layer_type[name] = type; tk.skip_statement(); }
                else if (u == "WIDTH") { std::string v; tk.next(v); if (is_num(v)) lef.layer_width[name] = atof(v.c_str()); tk.skip_statement(); }
                else if (u != ";") tk.skip_statement();
            }
        } else if (t == "VIA") {
            std::string name; tk.next(name);
            LefVia v;
            std::string cur;
            while (tk.next(u)) {
                if (u == "END") { tk.next(u); if (u == name) break; continue; }
                if (u == "LAYER") { tk.next(cur); tk.skip_statement(); }
                else if (u == "RECT") { double r[4]; if (read_rect4(tk, r)) v.rects.push_back({cur, {r[0], r[1], r[2], r[3]}}); }
                else if (u == "DEFAULT" || u == "GENERATED") { }
                else if (u != ";") tk.skip_statement();
            }
            lef.vias[name] = v;
        } else if (t == "MACRO") {
            std::string name; tk.next(name);
            LefMacro m;
            std::string cur;
            bool in_pin = false, in_obs = false;
            std::string pin_name;
            while (tk.next(u)) {
                if (u == "END") {
                    tk.peek(t);
                    if (in_obs && (t != pin_name || pin_name.empty())) { in_obs = false; continue; }   // END of OBS
                    if (in_pin && t == pin_name) { tk.next(t); in_pin = false; continue; }
                    if (t == name) { tk.next(t); break; }
                    continue;   // END of PORT
                }
                if (u == "SIZE") { std::string a, by, b; tk.next(a); tk.next(by); tk.next(b); m.w = atof(a.c_str()); m.h = atof(b.c_str()); m.has_size = true; tk.skip_statement(); }
                else if (u == "ORIGIN") { std::string a, b; tk.next(a); tk.next(b); m.ox = atof(a.c_str()); m.oy = atof(b.c_str()); tk.skip_statement(); }
                else if (u == "PIN") { tk.next(pin_name); in_pin = true; in_obs = false; }
                else if (u == "OBS") { in_obs = true; in_pin = false; pin_name.clear(); }
                else if (u == "PORT") { }
                else if (u == "LAYER") { tk.next(cur); tk.skip_statement(); }
                else if (u == "RECT") { double r[4]; if (read_rect4(tk, r)) m.rects.push_back({cur, r[0], r[1], r[2], r[3], in_pin}); }
                else if (u == "POLYGON") { ++m.n_polygons; tk.skip_statement(); }
                else if (u == "VIA") { tk.skip_statement(); }
                else if (u == "FOREIGN" || u == "CLASS" || u == "SYMMETRY" || u == "SITE" || u == "DIRECTION" ||
                         u == "USE" || u == "SHAPE" || u == "PROPERTY" || u == "SOURCE" || u == "EEQ" ||
                         u.rfind("ANTENNA", 0) == 0 || u == "TAPERRULE" || u == "MUSTJOIN" || u == "NETEXPR" ||
                         u == "SUPPLYSENSITIVITY" || u == "GROUNDSENSITIVITY" || u == "DENSITY" || u == "FIXEDMASK" || u == "WIDTH") {
                    tk.skip_statement();
                }
            }
            lef.macros[name] = std::move(m);
        } else if (t == "VIARULE" || t == "SITE" || t == "NONDEFAULTRULE") {
            std::string name; tk.next(name); skip_to_end(tk, name);
        } else if (t == "PROPERTYDEFINITIONS") {
            skip_to_end(tk, "PROPERTYDEFINITIONS");
        } else if (t == "UNITS") {
            skip_to_end(tk, "UNITS");
        } else if (t == "END") {
            tk.next(u);   // END LIBRARY
        } else if (t != ";") {
            tk.skip_statement();
        }
    }
    return true;
}

// ------------------------------------------------------------------ DEF reader
// Streams a DEF and turns its geometry into calls of the virtual hooks below (DEF units):
//   probe_rect(src, layer, rect)  a wire rectangle, via rectangle, RECT or PIN shape
//   probe_point(src, layer, x, y) a point the geometry must cover (wire ends / middles, vias
//                                 without definition, + POLYGON vertices)
//   mark_incomplete(layer)        a DEF shape on layer that has no exact rectangle
// While a net / pin is read, cur_net / cur_special / cur_pin name it; net_end() and
// pin_end() hand over the net's / pin's attributes. Components are collected in comps.
struct DefReader {
    virtual ~DefReader() = default;
    virtual void probe_point(int, int, int64_t, int64_t, const char*) {}
    virtual void probe_rect(int, int, const Rect&, const char*) {}
    virtual void mark_incomplete(int) {}
    // net: attrs = "+ USE SIGNAL" etc. verbatim, props = + PROPERTY pairs, conns = "inst pin"
    virtual void net_end(const std::string&, bool, const std::vector<std::string>&,
                         const std::vector<std::pair<std::string, std::string>>&, const std::vector<std::string>&) {}
    virtual void pin_end(const std::string&, const std::vector<std::string>&) {}
    std::string cur_net, cur_pin;
    bool cur_special = false;
    // DEF statements kept verbatim (VERSION, DIVIDERCHAR, BUSBITCHARS, ROW, TRACKS,
    // GCELLGRID, PROPERTYDEFINITIONS, and sections not modelled: REGIONS, GROUPS, ...)
    bool keep_statements = false;
    std::vector<std::pair<std::string, std::string>> statements;

    std::vector<StackLayer> stack;
    std::unordered_map<std::string, int> lidx;           // stack layer name -> index
    Lef lef;
    double scale = 1.0;                                   // DEF dbu -> layout dbu
    int64_t def_units = 0;
    std::unordered_map<std::string, ViaDef> vias;         // DEF VIAS + LEF vias (DEF units)

    // probe statistics per source kind / layer
    enum Src { WIRE, SWIRE, VIA_, RECT_, PIN, NSRC };
    const char* src_name(int s) const { static const char* n[] = {"wire", "special-wire", "via", "rect", "pin"}; return n[s]; }
    std::map<std::string, int64_t> unknown_layers;        // DEF layer name not in stack
    std::map<std::string, int64_t> unknown_vias;
    int64_t n_via_fallback = 0;
    int64_t n_wires = 0, n_swires = 0, n_vias = 0, n_rects = 0, n_pins = 0, n_spolys = 0;
    std::map<std::string, int64_t> via_count;

    // DEF data
    std::string design;
    Rect die{0, 0, 0, 0};
    bool has_die = false;
    // a placed component; status PLACED/FIXED/COVER; attrs: the other "+ KEYWORD ..." clauses
    // (SOURCE, WEIGHT, HALO, ...) verbatim; props: "+ PROPERTY name value" pairs
    struct Comp { std::string macro; int64_t x = 0, y = 0; int orient = 0; std::string name, status;
                  std::vector<std::string> attrs; std::vector<std::pair<std::string, std::string>> props; };
    std::vector<Comp> comps;
    int64_t n_comp_unplaced = 0;

    int64_t L(int64_t v) const { return (int64_t)std::llround((double)v * scale); }

    int layer_of(const std::string& name) {
        auto it = lidx.find(name);
        if (it == lidx.end()) { ++unknown_layers[name]; return -1; }
        return it->second;
    }
    // via placed at (x, y) in DEF units; returns the layer the route continues on (or -1)
    int place_via(const std::string& name, int64_t x, int64_t y, int orient, int cur_layer) {
        ++n_vias; ++via_count[name];
        auto it = vias.find(name);
        if (it != vias.end() && !it->second.rects.empty()) {
            int other = -1;
            if (it->second.has_polygon) for (auto& lr : it->second.rects) { auto li = lidx.find(lr.first); if (li != lidx.end()) mark_incomplete(li->second); }
            for (auto& lr : it->second.rects) {
                auto li = lidx.find(lr.first);
                if (li == lidx.end()) { ++unknown_layers[lr.first]; continue; }
                if (stack[li->second].cut && !it->second.cuts_exact) { mark_incomplete(li->second); continue; }
                Rect r = orient_rect(orient < 0 ? 0 : orient, lr.second, x, y);
                probe_rect(VIA_, li->second, r, name.c_str());
                if (!stack[li->second].cut && li->second != cur_layer) other = li->second;
            }
            return other;
        }
        // stack rule: stated layer, next cut, next routing layer, at the via centre
        ++unknown_vias[name];
        ++n_via_fallback;
        if (cur_layer < 0 || cur_layer + 2 >= (int)stack.size()) return -1;
        for (int k = 0; k < 3; ++k) mark_incomplete(cur_layer + k);
        probe_point(VIA_, cur_layer, x, y, name.c_str());
        probe_point(VIA_, cur_layer + 1, x, y, name.c_str());
        probe_point(VIA_, cur_layer + 2, x, y, name.c_str());
        return cur_layer + 2;
    }
    void wire(bool special, int li, int64_t x1, int64_t y1, int64_t x2, int64_t y2, int64_t width,
              int64_t e1, int64_t e2) {
        int src = special ? SWIRE : WIRE;
        if (special) ++n_swires; else ++n_wires;
        if (li < 0) return;
        probe_point(src, li, x1, y1, "endpoint");
        probe_point(src, li, x2, y2, "endpoint");
        probe_point(src, li, (x1 + x2) / 2, (y1 + y2) / 2, "midpoint");
        if (width <= 0) { if (!(special && x1 == x2 && y1 == y2)) mark_incomplete(li); return; }
        int64_t hw = width / 2;
        if (e1 < 0) e1 = special ? 0 : hw;
        if (e2 < 0) e2 = special ? 0 : hw;
        Rect r;
        if (y1 == y2) {
            int64_t a = std::min(x1, x2), b = std::max(x1, x2);
            int64_t ea = (x1 <= x2) ? e1 : e2, eb = (x1 <= x2) ? e2 : e1;
            r = {a - ea, y1 - hw, b + eb, y1 + hw};
        } else if (x1 == x2) {
            int64_t a = std::min(y1, y2), b = std::max(y1, y2);
            int64_t ea = (y1 <= y2) ? e1 : e2, eb = (y1 <= y2) ? e2 : e1;
            r = {x1 - hw, a - ea, x1 + hw, b + eb};
        } else { mark_incomplete(li); return; }   // diagonal: points only
        probe_rect(src, li, r, "wire-rect");
    }
};

// ------------------------------------------------------------------ DEF parsing
bool expect(Tokenizer& tk, const char* what) {
    std::string t;
    return tk.next(t) && t == what;
}

// ( x y [ext] ) with '*' = previous value; on entry the '(' has been consumed
bool read_point(Tokenizer& tk, int64_t& x, int64_t& y, int64_t& ext, bool& has_ext) {
    std::string a, b, c;
    if (!tk.next(a) || !tk.next(b)) return false;
    if (a != "*") x = atoll(a.c_str());
    if (b != "*") y = atoll(b.c_str());
    has_ext = false;
    if (!tk.next(c)) return false;
    if (c != ")") { ext = atoll(c.c_str()); has_ext = true; if (!tk.next(c) || c != ")") return false; }
    return true;
}

bool is_orient(const std::string& t) { return orient_code(t) >= 0; }

// Tokens up to the next ';' (consumed), joined by single spaces.
std::string read_statement(Tokenizer& tk) {
    std::string t, out;
    while (tk.next(t) && t != ";") { if (!out.empty()) out += ' '; out += t; }
    return out;
}
// Tokens up to "END <name>" (consumed), joined; ';' kept so statements stay apart.
std::string read_to_end(Tokenizer& tk, const std::string& name) {
    std::string t, u, out;
    while (tk.next(t)) {
        if (t == "END") { if (!tk.next(u)) break; if (u == name) break; if (!out.empty()) out += ' '; out += "END " + u; continue; }
        if (!out.empty() && t != ";") out += ' ';
        out += t;
    }
    return out;
}
// One "+ KEYWORD ..." clause after the '+' and keyword were read: the tokens up to the next
// '+' or ';' (both left in the stream).
std::string read_clause(Tokenizer& tk) {
    std::string t, out;
    while (tk.peek(t) && t != "+" && t != ";") { tk.next(t); if (!out.empty()) out += ' '; out += t; }
    return out;
}
// "+ PROPERTY name value [name value ...]" after the keyword: pairs up to the next '+' / ';'.
void read_props(Tokenizer& tk, std::vector<std::pair<std::string, std::string>>& props) {
    std::string a, b;
    while (tk.peek(a) && a != "+" && a != ";") {
        tk.next(a);
        if (!tk.peek(b) || b == "+" || b == ";") { props.push_back({a, ""}); break; }
        tk.next(b);
        props.push_back({a, b});
    }
}

// Parses one wiring statement after ROUTED/FIXED/COVER/NOSHIELD/SHIELD <net> (consumed).
// Returns with the next '+' (pushed back) or ';' (pushed back).
void parse_wiring(Tokenizer& tk, DefReader& ck, bool special) {
    std::string t;
    int li = -1;
    int64_t width = -1;
    bool have_prev = false;
    int64_t px = 0, py = 0, pext = -1;
    auto start_layer = [&](bool read_width) {
        std::string ln; tk.next(ln);
        li = ck.layer_of(ln);
        width = -1;
        if (read_width) { std::string w; tk.peek(w); if (is_num(w)) { tk.next(w); width = atoll(w.c_str()); } }
        if (!special && li >= 0 && ck.stack[li].lef_width_um > 0)
            width = (int64_t)std::llround(ck.stack[li].lef_width_um * (double)ck.def_units);
        have_prev = false; pext = -1;
    };
    start_layer(special);
    while (tk.next(t)) {
        if (t == ";") { tk.push_back(t); return; }
        if (t == "+") {
            std::string k; tk.peek(k);
            if (k == "SHAPE" || k == "STYLE" || k == "MASK") { tk.next(k); tk.next(k); continue; }
            tk.push_back("+");   // back in front of the peeked keyword
            return;
        }
        if (t == "NEW") { start_layer(special); continue; }
        if (t == "TAPER") continue;
        if (t == "TAPERRULE" || t == "STYLE") { tk.next(t); continue; }
        if (t == "MASK") { tk.next(t); continue; }
        if (t == "(") {
            int64_t x = px, y = py, ext = -1; bool has_ext = false;
            if (!read_point(tk, x, y, ext, has_ext)) return;
            if (!has_ext) ext = -1;
            if (have_prev) ck.wire(special, li, px, py, x, y, width, pext, ext);
            px = x; py = y; pext = ext; have_prev = true;
            continue;
        }
        if (t == "RECT") {
            int64_t r[4];
            if (!expect(tk, "(")) return;
            for (int i = 0; i < 4; ++i) { tk.next(t); r[i] = atoll(t.c_str()); }
            expect(tk, ")");
            ++ck.n_rects;
            if (li >= 0) ck.probe_rect(DefReader::RECT_, li, {px + r[0], py + r[1], px + r[2], py + r[3]}, "RECT");
            continue;
        }
        if (t == "VIRTUAL") {
            int64_t x = px, y = py, ext = -1; bool he;
            expect(tk, "("); read_point(tk, x, y, ext, he);
            px = x; py = y; have_prev = true;
            continue;
        }
        if (t == "DO") {   // special-net via array: DO n BY m STEP dx dy (applies to last via)
            for (int i = 0; i < 5; ++i) tk.next(t);
            continue;
        }
        // a via name, optionally followed by an orientation
        int orient = -1;
        std::string o; tk.peek(o);
        if (is_orient(o)) { tk.next(o); orient = orient_code(o); }
        int other = ck.place_via(t, px, py, orient, li);
        if (other >= 0) { li = other; if (!special && ck.stack[li].lef_width_um > 0) width = (int64_t)std::llround(ck.stack[li].lef_width_um * (double)ck.def_units); }
    }
}

void parse_vias(Tokenizer& tk, DefReader& ck) {
    std::string t;
    while (tk.next(t)) {
        if (t == "END") { tk.next(t); return; }
        if (t != "-") continue;
        std::string name; tk.next(name);
        ViaDef v;
        std::string rule, bot, cut, topl;
        int64_t csx = 0, csy = 0, spx = 0, spy = 0, ebx = 0, eby = 0, etx = 0, ety = 0, rows = 1, cols = 1;
        int64_t ox = 0, oy = 0, obx = 0, oby = 0, otx = 0, oty = 0;
        bool pattern = false;
        auto num = [&]() { std::string s; tk.next(s); return (int64_t)atoll(s.c_str()); };
        while (tk.next(t) && t != ";") {
            if (t != "+") continue;
            tk.next(t);
            if (t == "RECT") {
                std::string ln; tk.next(ln);
                std::string m; tk.peek(m);
                if (m == "+") { tk.next(m); tk.next(m); tk.next(m); }   // + MASK n
                int64_t r[4]; expect(tk, "("); r[0] = num(); r[1] = num(); expect(tk, ")");
                expect(tk, "("); r[2] = num(); r[3] = num(); expect(tk, ")");
                v.rects.push_back({ln, {std::min(r[0], r[2]), std::min(r[1], r[3]), std::max(r[0], r[2]), std::max(r[1], r[3])}});
            } else if (t == "POLYGON") {
                std::string ln; tk.next(ln);
                int64_t xa = INT64_MAX, ya = INT64_MAX, xb = INT64_MIN, yb = INT64_MIN;
                std::string s;
                while (tk.peek(s) && s == "(") { tk.next(s); int64_t x = num(), y = num(); expect(tk, ")");
                    xa = std::min(xa, x); ya = std::min(ya, y); xb = std::max(xb, x); yb = std::max(yb, y); }
                (void)xa;   // polygon via shapes: not probed (bbox is not exact)
                v.has_polygon = true;
                v.rects.push_back({ln, {0, 0, 0, 0}});   // degenerate: names the layer only
            } else if (t == "VIARULE") { tk.next(rule); }
            else if (t == "CUTSIZE") { csx = num(); csy = num(); }
            else if (t == "LAYERS") { tk.next(bot); tk.next(cut); tk.next(topl); }
            else if (t == "CUTSPACING") { spx = num(); spy = num(); }
            else if (t == "ENCLOSURE") { ebx = num(); eby = num(); etx = num(); ety = num(); }
            else if (t == "ROWCOL") { rows = num(); cols = num(); }
            else if (t == "ORIGIN") { ox = num(); oy = num(); }
            else if (t == "OFFSET") { obx = num(); oby = num(); otx = num(); oty = num(); }
            else if (t == "PATTERN") { pattern = true; tk.next(t); }
        }
        if (!rule.empty()) {
            int64_t w = cols * csx + (cols - 1) * spx, h = rows * csy + (rows - 1) * spy;
            int64_t ax = -w / 2, ay = -h / 2;
            for (int64_t r = 0; r < rows; ++r)
                for (int64_t c = 0; c < cols; ++c) {
                    int64_t x1 = ax + c * (csx + spx) + ox, y1 = ay + r * (csy + spy) + oy;
                    v.rects.push_back({cut, {x1, y1, x1 + csx, y1 + csy}});
                }
            v.rects.push_back({bot, {ax - ebx + ox + obx, ay - eby + oy + oby, ax + w + ebx + ox + obx, ay + h + eby + oy + oby}});
            v.rects.push_back({topl, {ax - etx + ox + otx, ay - ety + oy + oty, ax + w + etx + ox + otx, ay + h + ety + oy + oty}});
            v.cuts_exact = !pattern;
        }
        ck.vias[name] = v;
    }
}

void parse_components(Tokenizer& tk, DefReader& ck) {
    std::string t;
    while (tk.next(t)) {
        if (t == "END") { tk.next(t); return; }
        if (t != "-") continue;
        DefReader::Comp c;
        tk.next(c.name); tk.next(c.macro);
        bool placed = false;
        while (tk.next(t) && t != ";") {
            if (t != "+") continue;
            tk.next(t);
            if (t == "PLACED" || t == "FIXED" || t == "COVER") {
                expect(tk, "(");
                int64_t x = 0, y = 0, e; bool he;
                read_point(tk, x, y, e, he);
                std::string o; tk.next(o);
                c.x = x; c.y = y; c.orient = orient_code(o);
                c.status = t;
                placed = true;
            } else if (t == "UNPLACED") {
                c.status = t;
            } else if (t == "PROPERTY") {
                read_props(tk, c.props);
            } else {
                std::string rest = read_clause(tk);
                c.attrs.push_back(rest.empty() ? t : t + " " + rest);
            }
        }
        if (placed) ck.comps.push_back(std::move(c)); else ++ck.n_comp_unplaced;
    }
}

void parse_pins(Tokenizer& tk, DefReader& ck) {
    std::string t;
    struct Shape { int li; Rect r; };
    while (tk.next(t)) {
        if (t == "END") { tk.next(t); return; }
        if (t != "-") continue;
        std::string pin; tk.next(pin);
        ck.cur_pin = pin;
        std::vector<std::string> attrs;
        std::vector<Shape> shapes;
        auto flush = [&](int64_t x, int64_t y, int orient) {
            for (auto& s : shapes) {
                ++ck.n_pins;
                if (s.li >= 0) ck.probe_rect(DefReader::PIN, s.li, orient_rect(orient, s.r, x, y), pin.c_str());
            }
            shapes.clear();
        };
        while (tk.next(t) && t != ";") {
            if (t != "+") continue;
            tk.next(t);
            if (t == "LAYER") {
                std::string ln; tk.next(ln);
                int li = ck.layer_of(ln);
                std::string s;
                while (tk.peek(s) && s != "(") { tk.next(s); }   // MASK n / SPACING v / DESIGNRULEWIDTH v
                int64_t x1 = 0, y1 = 0, x2 = 0, y2 = 0, e; bool he;
                expect(tk, "("); read_point(tk, x1, y1, e, he);
                expect(tk, "("); read_point(tk, x2, y2, e, he);
                shapes.push_back({li, {std::min(x1, x2), std::min(y1, y2), std::max(x1, x2), std::max(y1, y2)}});
            } else if (t == "PLACED" || t == "FIXED" || t == "COVER") {
                int64_t x = 0, y = 0, e; bool he;
                expect(tk, "("); read_point(tk, x, y, e, he);
                std::string o; tk.next(o);
                flush(x, y, std::max(0, orient_code(o)));
            } else if (t == "PORT") {
                // a new port: its own shapes and placement follow
            } else {
                std::string rest = read_clause(tk);   // NET n, SPECIAL, DIRECTION d, USE u, ...
                attrs.push_back(rest.empty() ? t : t + " " + rest);
            }
        }
        shapes.clear();   // unplaced pin shapes cannot be checked
        ck.pin_end(pin, attrs);
        ck.cur_pin.clear();
    }
}

void parse_nets(Tokenizer& tk, DefReader& ck, bool special) {
    std::string t;
    while (tk.next(t)) {
        if (t == "END") { tk.next(t); return; }
        if (t != "-") continue;
        std::string net; tk.next(net);
        ck.cur_net = net; ck.cur_special = special;
        std::vector<std::string> attrs, conns;
        std::vector<std::pair<std::string, std::string>> props;
        while (tk.next(t) && t != ";") {
            if (t == "(") {                  // connection: ( inst pin [+ SYNTHESIZED] ) / ( PIN name )
                std::string a, b, c, conn;
                tk.next(a); tk.next(b);
                conn = a + " " + b;
                while (tk.next(c) && c != ")") conn += " " + c;
                conns.push_back(conn);
                continue;
            }
            if (t != "+") continue;
            tk.next(t);
            if (t == "PROPERTY") { read_props(tk, props); continue; }
            if (t == "ROUTED" || t == "FIXED" || t == "COVER" || t == "NOSHIELD") {
                parse_wiring(tk, ck, special);
            } else if (t == "SHIELD") {
                tk.next(t);   // shield net name
                parse_wiring(tk, ck, special);
            } else if (special && t == "RECT") {
                std::string ln; tk.next(ln);
                int li = ck.layer_of(ln);
                std::string s;
                while (tk.peek(s) && s != "(") tk.next(s);
                int64_t x1 = 0, y1 = 0, x2 = 0, y2 = 0, e; bool he;
                expect(tk, "("); read_point(tk, x1, y1, e, he);
                expect(tk, "("); read_point(tk, x2, y2, e, he);
                ++ck.n_rects;
                if (li >= 0) ck.probe_rect(DefReader::RECT_, li, {std::min(x1, x2), std::min(y1, y2), std::max(x1, x2), std::max(y1, y2)}, "+RECT");
            } else if (special && t == "POLYGON") {
                std::string ln; tk.next(ln);
                int li = ck.layer_of(ln);
                std::string s;
                while (tk.peek(s) && s != "(" && s != "+" && s != ";") tk.next(s);
                int64_t x = 0, y = 0, e; bool he;
                while (tk.peek(s) && s == "(") {
                    tk.next(s); read_point(tk, x, y, e, he);
                    if (li >= 0) ck.probe_point(DefReader::RECT_, li, x, y, "+POLYGON vertex");
                }
                ck.mark_incomplete(li);
                ++ck.n_spolys;
            } else if (special && t == "VIA") {
                std::string vn; tk.next(vn);
                int orient = 0;
                std::string s; tk.peek(s);
                if (is_orient(s)) { tk.next(s); orient = orient_code(s); }
                int64_t x = 0, y = 0, e; bool he;
                while (tk.peek(s) && s == "(") { tk.next(s); read_point(tk, x, y, e, he); ck.place_via(vn, x, y, orient, -1); }
            } else if (t != "RECT" && t != "POLYGON" && t != "VIA") {
                std::string rest = read_clause(tk);   // USE, SOURCE, WEIGHT, NONDEFAULTRULE, ...
                attrs.push_back(rest.empty() ? t : t + " " + rest);
            }
        }
        ck.net_end(net, special, attrs, props, conns);
        ck.cur_net.clear();
    }
}

bool parse_def(const std::string& path, DefReader& ck, std::string& err) {
    Tokenizer tk;
    if (!tk.open(path)) { err = "cannot open DEF " + path; return false; }
    std::string t;
    while (tk.next(t)) {
        if (t == "DESIGN") { tk.next(ck.design); tk.skip_statement(); }
        else if (t == "VERSION" || t == "DIVIDERCHAR" || t == "BUSBITCHARS" || t == "TECHNOLOGY" ||
                 t == "ROW" || t == "TRACKS" || t == "GCELLGRID" || t == "HISTORY") {
            if (ck.keep_statements) ck.statements.push_back({t, read_statement(tk)});
            else tk.skip_statement();
        }
        else if (t == "UNITS") {
            std::string a, b, v; tk.next(a); tk.next(b); tk.next(v);
            ck.def_units = atoll(v.c_str()); tk.skip_statement();
            // LEF widths/vias are in microns: convert now
            for (auto& sl : ck.stack) { auto it = ck.lef.layer_width.find(sl.name); if (it != ck.lef.layer_width.end() && !sl.cut) sl.lef_width_um = it->second; }
            for (auto& kv : ck.lef.vias) {
                if (ck.vias.count(kv.first)) continue;
                ViaDef v;
                for (auto& r : kv.second.rects) {
                    double u = (double)ck.def_units;
                    v.rects.push_back({r.first, {(int64_t)std::llround(r.second[0] * u), (int64_t)std::llround(r.second[1] * u),
                                                 (int64_t)std::llround(r.second[2] * u), (int64_t)std::llround(r.second[3] * u)}});
                }
                ck.vias[kv.first] = v;
            }
        }
        else if (t == "DIEAREA") {
            int64_t xa = INT64_MAX, ya = INT64_MAX, xb = INT64_MIN, yb = INT64_MIN;
            std::string s;
            while (tk.next(s) && s != ";") {
                if (s != "(") continue;
                int64_t x = 0, y = 0, e; bool he;
                read_point(tk, x, y, e, he);
                xa = std::min(xa, x); ya = std::min(ya, y); xb = std::max(xb, x); yb = std::max(yb, y);
            }
            ck.die = {xa, ya, xb, yb}; ck.has_die = true;
        }
        else if (t == "VIAS") { tk.skip_statement(); parse_vias(tk, ck); }
        else if (t == "COMPONENTS") { tk.skip_statement(); parse_components(tk, ck); }
        else if (t == "PINS") { tk.skip_statement(); parse_pins(tk, ck); }
        else if (t == "SPECIALNETS") { tk.skip_statement(); parse_nets(tk, ck, true); }
        else if (t == "NETS") { tk.skip_statement(); parse_nets(tk, ck, false); }
        else if (t == "END") { tk.next(t); if (t == "DESIGN") break; }
        else if (t == "PROPERTYDEFINITIONS") {
            if (ck.keep_statements) ck.statements.push_back({t, read_to_end(tk, t)});
            else skip_to_end(tk, "PROPERTYDEFINITIONS");
        }
        else if (t == "NONDEFAULTRULES" || t == "REGIONS" || t == "GROUPS" || t == "BLOCKAGES" ||
                 t == "FILLS" || t == "SCANCHAINS" || t == "SLOTS" || t == "STYLES" || t == "PINPROPERTIES" ||
                 t == "COMPONENTMASKSHIFT" || t == "EXTENSION") {
            std::string sec = t;
            std::string count = read_statement(tk);
            if (ck.keep_statements) ck.statements.push_back({sec, count + " ; " + read_to_end(tk, sec)});
            else skip_to_end(tk, sec);
        }
        else if (t != ";") tk.skip_statement();
    }
    if (ck.def_units <= 0) { err = "DEF has no UNITS DISTANCE MICRONS"; return false; }
    return true;
}

} // namespace defr
