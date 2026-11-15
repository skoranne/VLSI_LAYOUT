#ifndef LEFDEF_EXPORT_H
#define LEFDEF_EXPORT_H

#include "layoutdb.h"
#include <string>
#include <vector>
#include <map>

struct LayerMapEntry {
    std::string name;   // LEF/DEF layer name, e.g. LI1, VIA, MET1, VIA1...
    std::string type;   // ROUTING or CUT
    int gds_layer = -1;
    int gds_datatype = -1;
    double width = 0.0; // routing width or cut size hint (microns, optional)
};

struct LefDefOptions {
    std::string design_name = "TOP";
    double dbu_per_micron = 1000.0; // DEF UNITS DISTANCE MICRONS
    // CONNECT stack in order: e.g. LI1 VIA MET1 VIA1 MET2 VIA2 MET3 VIA3 MET4 VIA4 MET5
    std::vector<std::string> connect_stack;
    std::vector<LayerMapEntry> layer_map;
    bool export_as_blockages = false; // if true emit BLOCKAGES, else SPECIALNETS shapes
};

// Build default Sky130-like mapping for the standard CONNECT stack:
// CONNECT LI1 VIA MET1 VIA1 MET2 VIA2 MET3 VIA3 MET4 VIA4 MET5
std::vector<LayerMapEntry> default_sky130_connect_map();

// Export OASIS/GDSII (opened via layoutdb) to LEF and DEF files.
// Returns 0 on success.
int export_lefdef(ldb_file_t* f, const std::string& lef_path, const std::string& def_path,
                  const LefDefOptions& opt, std::string& err);

#endif
