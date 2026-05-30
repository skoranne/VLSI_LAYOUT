////////////////////////////////////////////////////////////////////////////////
// File  : main.cpp
// Author: Sandeep Koranne
//
////////////////////////////////////////////////////////////////////////////////

// main.cpp ---------------------------------------------------------------

#include <iostream>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "gds_scanner.hpp"
using namespace VLSILayout;
int main(int argc, char* argv[])
{
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <gds-file>\n";
    return EXIT_FAILURE;
  }

  const char* path = argv[1];

  try {
    // 1️⃣ memory‑map the file
    MappedFile mf(path);

    // 2️⃣ container that will receive the cells
    std::map<std::string, Cell*> cells;

    // 3️⃣ scan the file
    scan_gds(mf, cells);

    // 4️⃣ report what we found
    std::cout << "Found " << cells.size() << " structure(s):\n";
    for (const auto& kv : cells) {
      kv.second->dump();          // print each cell
    }

    // 5️⃣ clean‑up (delete the Cell objects)
    for (auto& kv : cells) delete kv.second;
  }
  catch (const std::exception& ex) {
    std::cerr << "Error: " << ex.what() << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
