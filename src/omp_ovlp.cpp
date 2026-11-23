// File    : thrust_boolean.cu
// Author  : Sandeep Koranne (C) 2026
// Purpose : Use CUDA/Thrust to develop boolean operation on GPU
#ifdef INSTRUCTIONS

Given type(XYTracker) type :: XYTracker
integer(kind=K_COORDINATE_KIND) :: X, Y integer(kind=int8) ::
polygonNumber end and ! Auxiliary data structure for the sweep-line
type :: Event integer(kind=K_COORDINATE_KIND) :: x, y1, y2
integer(kind=int8) :: lap_change ! +1 for left edge, -1 for right
edge integer :: owner ! 1 for Shape A, 2 for Shape B end type Event

call sort_event_trackers( trackers )
We have
a GPU with 128 GB unified memory; we can load these rectangles which
are type(Box) with integer(kind=K_COORDINATE_KIND) x1,y1,x2,y2 end
and generate either the Vertex Corner trackers as well as the Sweep
line event for x-coordinate and y1 and y2. Using CUDA devise a plan
to perform Boolean operations on 2 collections of boxes such as
UNION_MERGE, AND, OR2, XOR, NOT, SIZE (Minkowski erosion). I have
examples with coordinates such as (0,0) (10,10) and second box is
(5,5) to (20,20). Generate CUDA code using Thrust/CUB which can work
on 100 million such boxes. I have a RTree which I will use on the
Host side to do some partitioning.
#endif

#include <iostream>
#include <vector>
#include <iostream>
#include <vector>

#include <fstream>
#include <cstdint>
#include <cassert>
#include <stdexcept>

// Assuming the Box struct from our GPU code
using K_COORDINATE_KIND = int32_t;

struct Box {
    K_COORDINATE_KIND x1, y1, x2, y2;
    int owner; // 1 for Shape A, 2 for Shape B
    int id;    // To track the original box or write output IDs
};

// Struct to hold pairs of boxes that need their overlaps resolved
struct OverlapPair {
    int box_a_id;
    int box_b_id;
};


// High-performance binary reader
std::vector<Box> read_boxes_from_binary(const std::string& filename, int owner_id) {
    // Open file at the end (ate) to easily get the file size
    std::ifstream file(filename, std::ios::binary | std::ios::ate);
    
    if (!file.is_open()) {
        throw std::runtime_error("Failed to open file: " + filename);
    }

    // Determine file size
    std::streamsize file_size = file.tellg();
    file.seekg(0, std::ios::beg);

    // Each rectangle consists of 4 int32_t values (16 bytes total)
    size_t rect_size = 4 * sizeof(K_COORDINATE_KIND);
    
    // Safety check to ensure the file isn't corrupted or incomplete
    if (file_size % rect_size != 0) {
        throw std::runtime_error("File size is not a multiple of 16 bytes. Corrupt data?");
    }

    size_t num_rects = file_size / rect_size;
    
    // Allocate a raw buffer to read the entire file in one single I/O operation
    std::vector<K_COORDINATE_KIND> raw_data(num_rects * 4);
    
    std::cout << "Reading " << num_rects << " rectangles (" 
              << (file_size / (1024.0 * 1024.0)) << " MB) from " << filename << "...\n";

    if (!file.read(reinterpret_cast<char*>(raw_data.data()), file_size)) {
         throw std::runtime_error("Failed to read binary data.");
    }

    // Allocate the final Host vector
    std::vector<Box> boxes(num_rects);

    // Process the raw data, run assertions, and format into the Box struct
    for (size_t i = 0; i < num_rects; ++i) {
        K_COORDINATE_KIND x1 = raw_data[i * 4 + 0];
        K_COORDINATE_KIND y1 = raw_data[i * 4 + 1];
        K_COORDINATE_KIND x2 = raw_data[i * 4 + 2];
        K_COORDINATE_KIND y2 = raw_data[i * 4 + 3];

        // Ensure valid box geometry
        // Note: assertions are removed in Release builds (when NDEBUG is defined). 
        // If you need these checks to run in production, use standard if/throw statements.
        assert(x2 > x1 && "Assertion failed: x2 must be strictly greater than x1");
        assert(y2 > y1 && "Assertion failed: y2 must be strictly greater than y1");

        boxes[i] = {x1, y1, x2, y2, owner_id, static_cast<int>(i)};
    }
    
    return boxes;
}




/**
 * OpenMP Target Overlap Detector
 * Optimized for RTree Tiles (N < 2048)
 */
void detect_overlaps_omp(const Box* d_boxes,
			 int n,
                         int& out_count) {
    int count = 0;

    #pragma omp target data map(to: d_boxes[0:n]) map(tofrom: count) 
    {
        #pragma omp target teams loop reduction(+:count)
        for (int i = 0; i < n; ++i) {
            for (int j = i + 1; j < n; ++j) {
                
                // Pure geometric check
                bool overlap = (d_boxes[i].x1 < d_boxes[j].x2 && 
                                d_boxes[i].x2 > d_boxes[j].x1 && 
                                d_boxes[i].y1 < d_boxes[j].y2 && 
                                d_boxes[i].y2 > d_boxes[j].y1);

                if (overlap) {
		  count++;
		}
            }
        }
    }
    out_count = count;
}

int main(int argc, char* argv[]) {
  
  std::vector<Box> h_A =  read_boxes_from_binary(argv[1], 1);
  std::vector<Box> h_B =  read_boxes_from_binary(argv[2], 2);    
  int ovlp_count = 0;
  detect_overlaps_omp(&h_B[0],h_B.size(), ovlp_count);
  std::cout << "OVLP count = " << ovlp_count << std::endl;
  return 0;
}
