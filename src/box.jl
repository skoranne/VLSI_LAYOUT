################################################################################
# File   : box.jl
# Author : Sandeep Koranne (C) 2026
# Purpose: Julia plotting
################################################################################
using HDF5
using DataFrames
using SpatialIndexing
const SI = SpatialIndexing
# Open your specific HDF5 file
function LoadHDF5IntoDF(fileName)
    retval=h5open(fileName, "r") do file
        # Read the compound dataset. 
        # Julia reads HDF5 H5T_COMPOUND as a NamedTuple of vectors.
        boxes_data = read(file["boxes"])
        
        # Pass the NamedTuple directly into DataFrame to create columns automatically
        df = DataFrame(boxes_data)
        
        # 1. Print the DataFrame
        #println("DataFrame contents:")
        #println(df)
        
        # 2. Get the count of the boxes (as indicated by the metadata)
        box_count = nrow(df)
        println("\nTotal count of boxes: ", box_count)
        df
    end
    retval
end
# Sample data
# 1. FIX: Explicitly create SI.SpatialElem objects instead of Tuples.
# The parameters are: SpatialElem(rect, id, val)
# Since your tree has 'Nothing' for the ID type, we pass 'nothing' as the second argument.
function BulkLoadTree(df;leaf_capacity=16,branch_capacity=16)
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
    #tree = SimpleSpatialIndex{Int32, 2, SpatialItem{Float64, 2, Int, String}}()
    #tree = SI.SimpleSpatialIndex{Int32, 2, SI.SpatialElem{Int32, 2, Int64, Nothing}}(Int64; leaf_capacity, branch_capacity)

    #this does OMT
    tree = SI.RTree{Int32, 2}(Int64; leaf_capacity, branch_capacity)
    
    # 3. Bulk load the array of SpatialElems
    SI.load!(tree, spatial_items)
    
    println("Successfully bulk-loaded $(length(tree)) elements into the index.")
    tree
end

using SpatialIndexing
const SI = SpatialIndexing

# Function to recursively collect all items from leaf nodes in order
function get_leaf_order(node::Union{SI.Branch, SI.Leaf})
    items = []
    
    if node isa SI.Leaf
        # If the node is a Leaf type, its children are the raw SpatialItems
        for item in node.children
            push!(items, item)
        end
    else
        # If the node is a Branch type, recurse deeper into child branches/leaves
        for child_node in node.children
            append!(items, get_leaf_order(child_node))
        end
    end
    return items
end

# Convenient wrapper method for the tree object
get_leaf_order(tree::SI.RTree) = get_leaf_order(tree.root)

function CheckTree(df,tree)
# 1. Assuming you have initialized your tree and bulk loaded it
# tree = SI.RTree{Int32, 2}(Int64; leaf_capacity=10, branch_capacity=50)
# SI.load!(tree, spatial_items)

# 2. Extract the items in the order they reside across the leaf nodes
ordered_items = get_leaf_order(tree)

# 3. Pull out the IDs to explicitly check the index sequence
julia_omt_id_sequence = [item.val for item in ordered_items]

    
println("First 10 item IDs in Julia's OMT leaf order: ", julia_omt_id_sequence[1:min(10, end)])
    boxes = [
        BBox(id, Int32(r.x1), Int32(r.y1), Int32(r.x2), Int32(r.y2)) 
        for (id,r) in enumerate(eachrow(df))
            ]

    nothing
end

struct BBox
    id::Int
    xmin::Int32
    ymin::Int32
    xmax::Int32
    ymax::Int32
end

function PlotDFMBR(df; k::Int=32)
    # Initialize the plot
    p = plot(legend=false, aspect_ratio=:equal, title="RTree MBRs (k=$k)", yflip=true)
    
    boxes = [
        BBox(id, Int32(r.x1), Int32(r.y1), Int32(r.x2), Int32(r.y2)) 
        for (id,r) in enumerate(eachrow(df))
            ]
    # 2. Plot all individual boxes first (so they are in the background)
    for box in boxes
        rect = Shape([
            (box.xmin, box.ymin),
            (box.xmax, box.ymin),
            (box.xmax, box.ymax),
            (box.xmin, box.ymax)
        ])
        
        # Lighter colors for individual boxes
        #plot!(p, rect, fillalpha=0.2, linecolor=:gray, seriescolor=:lightblue)
        
        #xc = (box.xmin + box.xmax) / 2.0
        #yc = (box.ymin + box.ymax) / 2.0
        #annotate!(p, xc, yc, text(string(box.id), 3, :gray, :center))
    end
    
    # 3. Calculate and plot MBRs for every k boxes
    num_boxes = length(boxes)
    for i in 1:k:num_boxes
        # Get the group of k boxes (handling the final group if it has fewer than k)
        group = boxes[i:min(i+k-1, num_boxes)]
        
        # Calculate the MBR boundaries for the group
        mbr_xmin = minimum(b.xmin for b in group)
        mbr_ymin = minimum(b.ymin for b in group)
        mbr_xmax = maximum(b.xmax for b in group)
        mbr_ymax = maximum(b.ymax for b in group)
        
        mbr_rect = Shape([
            (mbr_xmin, mbr_ymin),
            (mbr_xmax, mbr_ymin),
            (mbr_xmax, mbr_ymax),
            (mbr_xmin, mbr_ymax)
        ])
        
        # Plot the MBR with a thicker red line and no fill
        plot!(p, mbr_rect, fillalpha=0.0, linecolor=:red, linewidth=1.0)
        
        # Add a label for the MBR group above its top edge
        mbr_xc = (mbr_xmin + mbr_xmax) / 2.0
        group_num = (i ÷ k) + 1
        # Subtracting from ymin puts the text slightly "above" the box because of yflip=true
        annotate!(p, mbr_xc, mbr_ymin - 5, text("MBR $group_num", 3, :red, :bottom))
    end
    
    # Show the plot
    display(p)
    return p
end
#=
julia> @time df=LoadHDF5IntoDF("/scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/SDT6x6_FLAT_L11.h5");                                                   

Total count of boxes: 310901868
  4.876336 seconds (84.34 k allocations: 9.270 GiB, 18.92% gc time, 1.51% compilation time)                                                      

julia> @time SDT_LI1tree=BulkLoadTree(df);
Successfully bulk-loaded 310901868 elements into the index.
753.376606 seconds (5.81 G allocations: 249.454 GiB, 9.56% gc time, 0.02% compilation time)                                                      

julia> @time SelfTestTree(df,SDT_LI1tree)
Self-test passed: All boxes were successfully found in the tree!
6141.528932 seconds (28.74 G allocations: 859.622 GiB, 3.92% gc time, 0.00% compilation time)                                                    

julia> @time SDT_LI1tree_128_128=BulkLoadTree(df;leaf_capacity=128,branch_capacity=128);
Successfully bulk-loaded 310901868 elements into the index.
695.703685 seconds (5.63 G allocations: 187.158 GiB, 12.45% gc time)

julia> @time SelfTestTree(df,SDT_LI1tree_128_128)
Self-test passed: All boxes were successfully found in the tree!
5271.933189 seconds (23.19 G allocations: 685.079 GiB, 4.14% gc time)

With MT
julia> @time SelfTestTree(df,tree)
Self-test passed: All boxes were successfully found in the tree!
422.064508 seconds (24.13 G allocations: 696.696 GiB, 35.99% gc time, 4.68% compilation time)                                                    

=#
function SelfTestTree(df,tree)
    #boxes = [
    #    BBox(id, Int32(r.x1), Int32(r.y1), Int32(r.x2), Int32(r.y2)) 
    #    for (id,r) in enumerate(eachrow(df))
    #        ]
    num_rows = size(df, 1)
    
    # Place the macro directly in front of the for-loop
    Threads.@threads for id in 1:num_rows
        r = df[id, :] # Extract the current row based on the index
        #@threads for (id,r) in enumerate(boxes)
        query_rect = SI.Rect((Int32(r.x1), Int32(r.y1)), (Int32(r.x2), Int32(r.y2)))
        
        # 2. Perform the spatial search
        # intersects_with returns an iterator of elements in the tree
        results_iterator = SI.intersects_with(tree, query_rect)
        
        # 3. Extract the IDs from the matched elements
        # Note: Depending on how you wrote your `insert!` logic, the ID from your 
        # dataframe might be stored in `el.id` or `el.val`. Adjust accordingly.
        found_ids = [el.val for el in results_iterator]        
        # Scenario A: If your tree search returns a list/array of IDs directly
        @assert id in found_ids "Self-test failed: Expected ID $id was not found in the search results!"     
    end
    println("Self-test passed: All boxes were successfully found in the tree!")
end
