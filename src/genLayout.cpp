////////////////////////////////////////////////////////////////////////////////
// File   : genLayout.cpp
// Author : Sandeep Koranne
// Purpose: Use KLayout's C++ API to perform layout generation
////////////////////////////////////////////////////////////////////////////////
#include <iostream>
#include "dbLayout.h"
//#include "dbCell.h"
//#include "dbShape.h"
//#include "dbBox.h"
//#include "dbLayer.h"
#include "dbLayerProperties.h"
#include "dbPolygon.h"
#include "dbWriter.h"
#include "tlStream.h"
#include "dbPlugin.h"


#if 0
int main() {
  try {
    // 1. Initialize a new layout instance
    db::Layout layout;

    // Set the database unit (e.g., 0.001 micrometers = 1 nanometer)
    layout.dbu(0.001);

    // 2. Create a new layer (Layer 1, Datatype 0)
    int diffusion = layout.insert_layer( db::LayerProperties( 65,20 ) );

    // 3. Create a top cell in the layout
    auto top_cell_index = layout.add_cell("TOP_CELL");
    auto top_cell = layout.take_cell( top_cell_index);
    // 4. Manipulate layout by adding shapes (coordinates are in database units)
        
    // Example A: Add a simple rectangle (Box)
    // Creating a box from (x1=1000, y1=1000) to (x2=5000, y2=4000) dbu
    db::Box box(1000, 1000, 5000, 4000);
    top_cell->shapes(diffusion).insert(box);

    // Example B: Add a polygon
    std::vector<db::Point> pts;
    pts.push_back(db::Point(6000, 1000));
    pts.push_back(db::Point(9000, 1000));
    pts.push_back(db::Point(9000, 4000));
    pts.push_back(db::Point(7500, 6000)); // Peak
    pts.push_back(db::Point(6000, 4000));
        
    db::Polygon poly;
    poly.assign_hull( pts.begin(), pts.end() );
    //top_cell->shapes(diffusion).insert(poly);

    // 5. Save the manipulated layout to a GDSII file
    std::string output_filename = "output_manipulated.gds";
    tl::OutputStream output_stream(output_filename);
    db::SaveLayoutOptions defaultSaveOptions;
    db::Writer writer( defaultSaveOptions );
    //writer.write(layout, output_stream);

    std::cout << "Layout manipulation successful! Saved to " << output_filename << std::endl;
  }
  catch (const std::exception& e) {
    std::cerr << "Error during layout manipulation: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
#endif

//template db::layer<db::Box, db::unstable_layer_tag>& 
//db::Shapes::get_layer<db::Box, db::unstable_layer_tag>();
int main() {
  try {
    db::FormatRegistry::instance().register_format("GDS2", "GDSII");    
    db::Manager m(true);
    db::Layout layout(&m);
    layout.dbu(0.001);
    int diffusion = layout.insert_layer( db::LayerProperties( 65,20 ) );
    db::cell_index_type top_cell_index = layout.add_cell("TOP_CELL");
    db::Cell* top_cell = layout.take_cell( top_cell_index);
    db::Box box(1000, 1000, 5000, 4000);
    //auto diff_shapes(  top_cell->shapes(diffusion) );
    top_cell->shapes(diffusion).insert(box);
    //diff_shapes.insert(box);

    //layout.write("x.gds");
    //return 0;
    // 5. Save the manipulated layout to a GDSII file
    std::string output_filename = "output_manipulated.gds";
    tl::OutputStream output_stream(output_filename);
    db::SaveLayoutOptions defaultSaveOptions;
    defaultSaveOptions.set_format("GDS2"); 
    db::Writer writer( defaultSaveOptions );
    writer.write(layout, output_stream);

    std::cout << "Layout manipulation successful! Saved to " << output_filename << std::endl;
  }
  catch (const tl::Exception& e) {
    // THIS is the critical line. It will tell us exactly why it's throwing.
    std::cerr << "KLayout Writer Exception: " << e.msg() << std::endl;
  }
  catch (const std::exception& e) {
    std::cerr << "Error during layout manipulation: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}

