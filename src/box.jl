################################################################################
# File   : box.jl
# Author : Sandeep Koranne (C) 2026
# Purpose: Julia plotting
################################################################################
using HDF5
using DataFrames
using SpatialIndexing

# Open your specific HDF5 file
h5open("SEDFXTP1_L8.h5", "r") do file
    # Read the compound dataset. 
    # Julia reads HDF5 H5T_COMPOUND as a NamedTuple of vectors.
    boxes_data = read(file["boxes"])
    
    # Pass the NamedTuple directly into DataFrame to create columns automatically
    df = DataFrame(boxes_data)
    
    # 1. Print the DataFrame
    println("DataFrame contents:")
    println(df)
    
    # 2. Get the count of the boxes (as indicated by the metadata)
    box_count = nrow(df)
    println("\nTotal count of boxes: ", box_count)
    df
end

const SI = SpatialIndexing

# Sample data
# 1. FIX: Explicitly create SI.SpatialElem objects instead of Tuples.
# The parameters are: SpatialElem(rect, id, val)
# Since your tree has 'Nothing' for the ID type, we pass 'nothing' as the second argument.
spatial_items = [
    SI.SpatialElem(
        SI.Rect((r.x1, r.y1), (r.x2, r.y2)), 
        nothing,   # ID slot (corresponds to Nothing)
        i          # Value slot (corresponds to Int64 row index)
    )
    for (i, r) in enumerate(eachrow(df))
]

# 2. Instantiate the tree matching the exact signature: 
# Coordinate type: Int32, Dimensions: 2, Value type: Int64
tree = SI.RTree{Int32, 2}(Int64)

# 3. Bulk load the array of SpatialElems
SI.load!(tree, spatial_items)

println("Successfully bulk-loaded $(length(tree)) elements into the index.")

