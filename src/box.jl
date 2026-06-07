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
function LoadHDF5IntoDF(fileName,datasetname)
    retval=h5open(fileName, "r") do file
        # Read the compound dataset. 
        # Julia reads HDF5 H5T_COMPOUND as a NamedTuple of vectors.
        boxes_data = read(file[datasetname])
        
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

#=
begin_lib 0.001
begin_cell {sky130_fd_sc_hd__sedfxbp_1}
box 236 0 {0 0} {14260 2720}
box 122 16 {145 -85} {315 85}
box 122 16 {145 -85} {315 85}
box 64 16 {145 2635} {315 2805}
box 64 16 {145 2635} {315 2805}
box 68 16 {145 -85} {315 85}
box 68 16 {145 2635} {315 2805}
boundary 66 20 {10205 105} {10205 1035} {10525 1035} {10525 705} {10355 705} {10355 105} {10205 105}
boundary 66 20 {10520 1605} {10520 1875} {10525 1875} {10525 2615} {10675 2615} {10675 1875} {10850 1875} {10850 1605} {10520 1605}
boundary 66 20 {10735 105} {10735 1245} {10040 1245} {10040 1575} {10105 1575} {10105 2615} {10255 2615} {10255 1575} {10310 1575} {10310 1395} {10885 1395} {10885 text 67 5 0 0 {11910 1190} {Q_N}
text 67 5 0 0 {13680 1190} {Q}
text 83 44 0 0 {7600 1870} {clkneg}
text 83 44 0 0 {8580 1345} {M0}
text 83 44 0 0 {9010 1345} {M1}
text 83 44 90 0 {0 0} {sedfxbp_1}
end_cell
end_lib
=#
function extract_and_split_rects(input_file::String)
    # Dictionary to hold our open file streams. 
    # Key: (layer, datatype), Value: IOStream
    out_files = Dict{Tuple{String, String}, IOStream}()
    
    try
        for line in eachline(input_file)
            line = strip(line)
            if isempty(line)
                continue
            end

            # 1. Extract the layer and datatype from the start of the line
            # Matches a word (box/boundary), spaces, number (layer), spaces, number (datatype)
            header_match = match(r"^[a-zA-Z]+\s+(\d+)\s+(\d+)", line)
            
            if header_match !== nothing
                layer = header_match.captures[1]
                datatype = header_match.captures[2]
                
                # 2. Dynamically create and open the file if we haven't seen this pair yet
                file_key = (layer, datatype)
                if !haskey(out_files, file_key)
                    filename = "L$(layer)_D$(datatype)_rect.txt"
                    out_files[file_key] = open(filename, "w")
                    println("Created new output file: $filename")
                end
                
                # Get the correct file stream for this line
                out_io = out_files[file_key]
                
                # 3. Extract coordinates and calculate the bounding box
                coord_matches = eachmatch(r"\{(-?\d+)\s+(-?\d+)\}", line)
                xs = Int[]
                ys = Int[]
                
                for m in coord_matches
                    push!(xs, parse(Int, m.captures[1]))
                    push!(ys, parse(Int, m.captures[2]))
                end
                
                # 4. Write to the specific layer/datatype file
                if !isempty(xs) && !isempty(ys)
                    min_x, max_x = minimum(xs), maximum(xs)
                    min_y, max_y = minimum(ys), maximum(ys)
                    
                    println(out_io, "rect $min_x $min_y $max_x $max_y")
                end
            end
        end
    finally
        # Safely close all open files regardless of whether the loop finishes normally or errors out
        for io in values(out_files)
            close(io)
        end
    end
    
    println("Conversion complete! All data routed to respective files.")
end

# === How to use it ===
# extract_and_split_rects("input.txt")



# Function to fracture a rectilinear polygon into rectangles using a sweep-line
function fracture_to_rects(xs::Vector{Int}, ys::Vector{Int})
    # Ensure the polygon is a closed loop
    if xs[1] != xs[end] || ys[1] != ys[end]
        push!(xs, xs[1])
        push!(ys, ys[1])
    end
    
    y_cuts = sort(unique(ys))
    rects = NTuple{4, Int}[]
    
    # Sweep horizontally across the polygon
    for i in 1:length(y_cuts)-1
        y_bottom = y_cuts[i]
        y_top = y_cuts[i+1]
        y_mid = (y_bottom + y_top) / 2.0  # Sample the midpoint of the horizontal slice
        
        active_xs = Int[]
        
        # Find all vertical edges that cross this horizontal slice
        for j in 1:length(xs)-1
            if xs[j] == xs[j+1] # It's a vertical edge
                ymin, ymax = minmax(ys[j], ys[j+1])
                if ymin < y_mid < ymax
                    push!(active_xs, xs[j])
                end
            end
        end
        
        # Sort the X coordinates intersecting this slice from left to right
        sort!(active_xs)
        
        # Pair them up (Left Edge, Right Edge) to form bounding rectangles
        for k in 1:2:length(active_xs)-1
            push!(rects, (active_xs[k], y_bottom, active_xs[k+1], y_top))
        end
    end
    
    return rects
end

#=
For MW_FLAT.oas
All HDF5 conversions completed successfully!
7533.780301 seconds (74.04 G allocations: 3.446 TiB, 8.09% gc time, 0.02% compilation time)                                   
=#
function convert_to_hdf5(input_file::String)
    # Accumulate all rectangles in memory first to do a single fast HDF5 write.
    # Key: (layer, datatype), Value: Array of (x1, y1, x2, y2) tuples
    layer_data = Dict{Tuple{String, String}, Vector{NTuple{4, Int}}}()
    
    for line in eachline(input_file)
        line = strip(line)
        if isempty(line)
            continue
        end

        # Extract layer and datatype
        header_match = match(r"^[a-zA-Z]+\s+(\d+)\s+(\d+)", line)
        
        if header_match !== nothing
            layer = header_match.captures[1]
            datatype = header_match.captures[2]
            file_key = (layer, datatype)
            
            # Initialize array for this layer/datatype if we haven't seen it
            if !haskey(layer_data, file_key)
                layer_data[file_key] = NTuple{4, Int}[]
            end
            
            # Extract coordinates
            coord_matches = eachmatch(r"\{(-?\d+)\s+(-?\d+)\}", line)
            xs = Int[]
            ys = Int[]
            
            for m in coord_matches
                push!(xs, parse(Int, m.captures[1]))
                push!(ys, parse(Int, m.captures[2]))
            end
            
            if isempty(xs) || isempty(ys)
                continue
            end
            
            # Process based on shape type
            if startswith(line, "box")
                # Box is already a rectangle, just get the bounds
                push!(layer_data[file_key], (minimum(xs), minimum(ys), maximum(xs), maximum(ys)))
            elseif startswith(line, "boundary")
                # Chop the polygon into rectangles
                rects = fracture_to_rects(xs, ys)
                append!(layer_data[file_key], rects)
            end
        end
    end
    
    # Write the accumulated data to HDF5 files
    println("Parsing complete. Writing HDF5 files...")
    for (key, rects) in layer_data
        layer, datatype = key
        filename = "gL$(layer)_D$(datatype).h5"
        
        # Convert array of tuples into an N x 4 Matrix for HDF5
        num_rects = length(rects)
        rect_matrix = zeros(Int, num_rects, 4)
        
        for i in 1:num_rects
            rect_matrix[i, 1] = rects[i][1] # x1
            rect_matrix[i, 2] = rects[i][2] # y1
            rect_matrix[i, 3] = rects[i][3] # x2
            rect_matrix[i, 4] = rects[i][4] # y2
        end
        
        # Create HDF5 file and write the dataset
        h5open(filename, "w") do file
            write(file, "rectangles", rect_matrix)
        end
        
        println("Saved $num_rects rectangles to $filename")
    end
    
    println("All HDF5 conversions completed successfully!")
end

# === How to use it ===
# convert_to_hdf5("input.txt")

function parse_rectangles_h5(file_path::String)
    # 1. Open the HDF5 file and read the dataset matrix
    data_matrix = h5open(file_path, "r") do file
        # This reads the "rectangles" dataset directly into a Julia Matrix
        read(file, "rectangles")
    end

    # Note on Dimensions: 
    # The HDF5 layout says (4, 149). 
    # In Julia (which is column-major), h5read typically maps this 
    # so that the first dimension (4) becomes rows, and the second (149) becomes columns.
    # Let's verify and safely handle the orientation.
    
    if size(data_matrix, 1) == 4
        # If rows = 4, columns = 149: Extract each row as a coordinate property
        df = DataFrame(
            x1 = data_matrix[1, :],
            y1 = data_matrix[2, :],
            x2 = data_matrix[3, :],
            y2 = data_matrix[4, :]
        )
    elseif size(data_matrix, 2) == 4
        # If it read transposed (rows = 149, columns = 4): Extract columns
        df = DataFrame(
            x1 = data_matrix[:, 1],
            y1 = data_matrix[:, 2],
            x2 = data_matrix[:, 3],
            y2 = data_matrix[:, 4]
        )
    else
        error("Dataset shape $(size(data_matrix)) does not have a dimension of size 4.")
    end

    return df
end

# Example usage:
# df = parse_rectangles_h5("gL67_D20.h5")
# first(df, 5)
