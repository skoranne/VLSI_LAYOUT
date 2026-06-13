################################################################################
# File   : box.jl
# Author : Sandeep Koranne (C) 2026
# Purpose: Julia plotting
################################################################################
using HDF5
using DataFrames
using SpatialIndexing
using CodecZstd
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

SDT16
julia> @time t=BulkLoadTree(df);
Successfully bulk-loaded 108331787 elements into the index.
220.979952 seconds (2.00 G allocations: 78.424 GiB, 7.94% gc time)                                                            

julia> @time SelfTestTree(df,t);
Self-test passed: All boxes were successfully found in the tree!                                                              
2399.523159 seconds (11.08 G allocations: 323.573 GiB, 2.60% gc time, 0.00% compilation time)                                 
With MT: 201.943511 seconds (11.23 G allocations: 331.378 GiB, 37.44% gc time, 12.35% compilation time)                                


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
using HDF5

function export_polygons_offset_to_hdf5(filename::String)
    raw_strings = [
        "boundary 66 20 {10205 105} {10205 1035} {10525 1035} {10525 705} {10355 705} {10355 105} {10205 105}",
        "boundary 66 20 {10520 1605} {10520 1875} {10525 1875} {10525 2615} {10675 2615} {10675 1875} {10850 1875} {10850 1605} {10520 1605}",
        "boundary 66 20 {10735 105} {10735 1245} {10040 1245} {10040 1575} {10105 1575} {10105 2615} {10255 2615} {10255 1575} {10310 1575} {10310 1395} {10885 1395} {10885 105} {10735 105}",
        "boundary 66 20 {11210 105} {11210 1035} {11095 1035} {11095 2615} {11245 2615} {11245 1220} {11580 1220} {11580 2615} {11730 2615} {11730 1220} {12535 1220} {12535 890} {12265 890} {12265 1035} {11845 1035} {11845 105} {11695 105} {11695 1035} {11360 1035} {11360 105} {11210 105}"
    ]

    X = Int32[]
    Y = Int32[]
    starts = Int32[]
    ends = Int32[]

    current_idx = 1
    for line in raw_strings
        matches = eachmatch(r"\{(\d+)\s+(\d+)\}", line)
        start_idx = current_idx
        
        for m in matches
            push!(X, parse(Int32, m.captures[1]))
            push!(Y, parse(Int32, m.captures[2]))
            current_idx += 1
        end
        
        # Record the 1-based Fortran-compatible offset bounds
        push!(starts, Int32(start_idx))
        push!(ends, Int32(current_idx - 1))
    end

    # Create two 2D matrices
    vertices_data = Matrix(hcat(X, Y)')        # Dim: 2 x N_vertices
    offsets_data  = Matrix(hcat(starts, ends)')  # Dim: 2 x N_polygons

    h5open(filename, "w") do file
        file["vertices"] = vertices_data
        file["offsets"]  = offsets_data
    end
    
    println("Wrote $(length(X)) vertices and $(length(starts)) polygon offsets to $filename")
end

#export_polygons_offset_to_hdf5("offset_polygons.h5")
function ConvertSTRMToHDF5(input_filename::String)
    # Dictionary to group arrays by (Layer, Datatype)
    # Key: (Int, Int) -> Value: Dict holding X, Y, starts, and ends arrays
    datasets = Dict{Tuple{Int, Int}, Dict{Symbol, Vector{Int32}}}()

    # Read the file line by line
    for line in eachline(input_filename)
        line = strip(line)
        isempty(line) && continue

        # --- Parse BOX elements ---
        if startswith(line, "box")
            # Regex extracts Layer, Datatype, and the two {x y} pairs
            m = match(r"box\s+(\d+)\s+(\d+)\s+\{(-?\d+)\s+(-?\d+)\}\s+\{(-?\d+)\s+(-?\d+)\}", line)
            if m !== nothing
                layer = parse(Int, m.captures[1])
                dtype = parse(Int, m.captures[2])
                x1, y1, x2, y2 = parse.(Int32, m.captures[3:6])

                # Initialize dictionary for this layer/datatype if it doesn't exist
                key = (layer, dtype)
                if !haskey(datasets, key)
                    datasets[key] = Dict(:X => Int32[], :Y => Int32[], :starts => Int32[], :ends => Int32[])
                end
                ds = datasets[key]

                start_idx = length(ds[:X]) + 1

                # Expand the 2 coordinate pairs into a 5-point closed polygon (CCW)
                push!(ds[:X], x1, x2, x2, x1, x1)
                push!(ds[:Y], y1, y1, y2, y2, y1)

                # Record 1-based Fortran offset
                push!(ds[:starts], Int32(start_idx))
                push!(ds[:ends], Int32(length(ds[:X])))
            end

        # --- Parse BOUNDARY elements ---
        elseif startswith(line, "boundary")
            # Regex extracts Layer, Datatype, and the remainder of the line
            m = match(r"boundary\s+(\d+)\s+(\d+)\s+(.+)", line)
            if m !== nothing
                layer = parse(Int, m.captures[1])
                dtype = parse(Int, m.captures[2])
                
                key = (layer, dtype)
                if !haskey(datasets, key)
                    datasets[key] = Dict(:X => Int32[], :Y => Int32[], :starts => Int32[], :ends => Int32[])
                end
                ds = datasets[key]

                start_idx = length(ds[:X]) + 1

                # Extract all coordinate pairs from the remainder string
                for cm in eachmatch(r"\{(-?\d+)\s+(-?\d+)\}", m.captures[3])
                    push!(ds[:X], parse(Int32, cm.captures[1]))
                    push!(ds[:Y], parse(Int32, cm.captures[2]))
                end

                # Record 1-based Fortran offset
                push!(ds[:starts], Int32(start_idx))
                push!(ds[:ends], Int32(length(ds[:X])))
            end
        end
    end

    # --- Write to HDF5 Files ---
    for ((layer, dtype), ds) in datasets
        filename = "L$(layer)_D$(dtype).h5"
        
        # Materialize standard-stride contiguous arrays using Matrix()
        # This prevents the "Cannot read/write arrays with a different stride" error
        vertices_data = Matrix(hcat(ds[:X], ds[:Y])')
        offsets_data  = Matrix(hcat(ds[:starts], ds[:ends])')
        
        h5open(filename, "w") do file
            file["vertices"] = vertices_data
            file["offsets"]  = offsets_data
        end
        
        println("Generated $filename: $(length(ds[:starts])) polygons, $(length(ds[:X])) vertices.")
    end
end

# To execute the script, just point it at your layout file:
# process_layout_to_hdf5("magic_layout.txt")
using Plots

function plot_contours(filename::String)
    # Store all parsed polygons as vectors of (X, Y) vectors
    all_x = Vector{Vector{Float64}}()
    all_y = Vector{Vector{Float64}}()
    
    current_x = Float64[]
    current_y = Float64[]
    flat_nums = Float64[]

    # 1. Parse the file
    for line in eachline(filename)
        line = strip(line)
        
        # Skip the header or empty lines
        if isempty(line) || startswith(line, "Extracted")
            continue
        end

        if startswith(line, "Polygon")
            # If we were already building a polygon, save it before starting the new one
            if !isempty(flat_nums)
                push!(all_x, flat_nums[1:2:end])
                push!(all_y, flat_nums[2:2:end])
                empty!(flat_nums)
            end
            
            # The polygon line contains numbers after the colon
            parts = split(line, ":")
            if length(parts) > 1
                nums = parse.(Float64, split(parts[2]))
                append!(flat_nums, nums)
            end
        else
            # This is a continuation line of pure numbers
            nums = parse.(Float64, split(line))
            append!(flat_nums, nums)
        end
    end

    # Don't forget to save the very last polygon in the file
    if !isempty(flat_nums)
        push!(all_x, flat_nums[1:2:end])
        push!(all_y, flat_nums[2:2:end])
    end

    # 2. Plot the polygons
    println("Successfully parsed $(length(all_x)) contours. Plotting...")
    
    # Initialize the plot canvas with equal aspect ratio to prevent stretching
    p = plot(legend=false, aspect_ratio=:equal, title="VLSI Contours", 
             xlabel="X", ylabel="Y", grid=true)

    for i in 1:length(all_x)
        xs = all_x[i]
        ys = all_y[i]
        
        # Ensure the polygon cycle is closed visually by repeating the first point
        if xs[1] != xs[end] || ys[1] != ys[end]
            push!(xs, xs[1])
            push!(ys, ys[1])
        end

        # Plot using :shape to fill the polygons. 
        # Alternatively, use seriestype=:path for outlines only.
        plot!(p, xs, ys, seriestype=:shape, fillalpha=0.4, linecolor=:black, linewidth=1.5)
    end

    display(p)
    return p
end

# Usage:
# plot_contours("my_contours.txt")
using Plots

# Define a structure to hold a complex shape
struct ComplexShape
    outer::Vector{Tuple{Float64, Float64}}
    holes::Vector{Vector{Tuple{Float64, Float64}}}
end

function plot_complex_contours(filename::String)
    shapes = ComplexShape[]
    
    flat_nums = Float64[]
    current_poly_type = 0 # 1 = outer boundary, >1 = hole
    
    # Helper function to convert flat array into (X, Y) tuples
    function flush_pts!()
        pts = Tuple{Float64, Float64}[]
        for i in 1:2:(length(flat_nums)-1)
            push!(pts, (flat_nums[i], flat_nums[i+1]))
        end
        # Ensure the cycle is visually closed
        if !isempty(pts) && pts[1] != pts[end]
            push!(pts, pts[1])
        end
        empty!(flat_nums)
        return pts
    end

    # Helper function to save the current gathered points into our structs
    function save_current!()
        pts = flush_pts!()
        if isempty(pts) return end
        
        if current_poly_type == 1
            # Start a brand new shape with an outer boundary
            push!(shapes, ComplexShape(pts, Vector{Vector{Tuple{Float64, Float64}}}()))
        elseif current_poly_type > 1
            # Add this as a hole to the most recently created shape
            if !isempty(shapes)
                push!(shapes[end].holes, pts)
            end
        end
    end

    # 1. Parse the file
    for line in eachline(filename)
        line = strip(line)
        if isempty(line) || startswith(line, "Extracted")
            continue
        end
        
        if startswith(line, "Polygon")
            save_current!() # Save whatever we were previously building
            
            parts = split(line, ":")
            header = split(parts[1])
            current_poly_type = parse(Int, header[2]) # "1", "2", etc.
            
            if length(parts) > 1
                append!(flat_nums, parse.(Float64, split(parts[2])))
            end
        else
            append!(flat_nums, parse.(Float64, split(line)))
        end
    end
    save_current!() # Don't forget the very last polygon!

    # 2. Plotting
    println("Successfully parsed $(length(shapes)) complex shapes. Plotting...")
    
    p = plot(legend=false, aspect_ratio=:equal, title="VLSI Layout with Holes", 
             xlabel="X", ylabel="Y", grid=true)
             
    # Pass 1: Draw all solid outer boundaries (Blue)
    for shape in shapes
        xs = [pt[1] for pt in shape.outer]
        ys = [pt[2] for pt in shape.outer]
        plot!(p, xs, ys, seriestype=:shape, fillcolor=:steelblue, fillalpha=0.7, linecolor=:black)
    end
    
    # Pass 2: Draw all holes over them (White out)
    for shape in shapes
        for hole in shape.holes
            hx = [pt[1] for pt in hole]
            hy = [pt[2] for pt in hole]
            # Use background color (white) with 100% opacity to punch the hole
            plot!(p, hx, hy, seriestype=:shape, fillcolor=:white, fillalpha=1.0, linecolor=:black)
        end
    end

    display(p)
    return p
end

# Usage:
# plot_complex_contours("layout.txt")

using HDF5


# (Assuming fracture_to_rects is defined elsewhere in your code)
# function fracture_to_rects(xs::Vector{Int}, ys::Vector{Int}) ...

function simple_convert_layout_to_hdf5(input_txt::String, h5_filename::String, xmf_filename::String)
    boxes = BoxData[]

    println("Parsing layout file...")
    for line in eachline(input_txt)
        line = strip(line)
        
        if startswith(line, "box")
            # Regex to extract the two coordinate pairs
            #m = match(r"box\s+\d+\s+\d+\s+\{([^}]+)\}\s+\{([^}]+)\}", line)
            m = match(r"box\s+.*?\{([^}]+)\}\s*\{([^}]+)\}", line)
            if m !== nothing
                pt1 = parse.(Int32, split(m[1]))
                pt2 = parse.(Int32, split(m[2]))
                
                # Ensure x1, y1 is the lower-left and x2, y2 is upper-right
                x1, x2 = min(pt1[1], pt2[1]), max(pt1[1], pt2[1])
                y1, y2 = min(pt1[2], pt2[2]), max(pt1[2], pt2[2])
                
                push!(boxes, BoxData(x1, y1, x2, y2))
            end
            
        elseif startswith(line, "boundary")
            # Extract all coordinate pairs inside {}
            xs = Int[]
            ys = Int[]
            for m in eachmatch(r"\{([^}]+)\}", line)
                coords = parse.(Int, split(m[1]))
                push!(xs, coords[1])
                push!(ys, coords[2])
            end
            
            # Pass to your existing fracturing function
            if length(xs) > 0
                fractured_rects = fracture_to_rects(xs, ys)
                for r in fractured_rects
                    # Assuming fracture_to_rects returns tuples or structs of (x1, y1, x2, y2)
                    push!(boxes, BoxData(Int32(r[1]), Int32(r[2]), Int32(r[3]), Int32(r[4])))
                end
            end
        end
    end

    num_boxes = length(boxes)
    println("Successfully parsed $num_boxes rectangles.")
    # --- FAIL-SAFE ---
    if num_boxes == 0
        println("ERROR: No boxes were found. Aborting HDF5 generation to prevent empty files.")
        return
    end
    # 2. Generate ParaView Mesh Data
    # ParaView needs 3D points (Z=0) and Quadrilateral connectivity
    points = zeros(Float32, 3, num_boxes * 4) # 3 coords (X,Y,Z), 4 points per box
    cells = zeros(Int32, 4, num_boxes)        # 4 point indices per quadrilateral

    for i in 1:num_boxes
        b = boxes[i]
        base_idx = (i - 1) * 4
        
        # Point 0: Bottom-Left
        points[1, base_idx + 1] = b.x1; points[2, base_idx + 1] = b.y1; points[3, base_idx + 1] = 0.0
        # Point 1: Bottom-Right
        points[1, base_idx + 2] = b.x2; points[2, base_idx + 2] = b.y1; points[3, base_idx + 2] = 0.0
        # Point 2: Top-Right
        points[1, base_idx + 3] = b.x2; points[2, base_idx + 3] = b.y2; points[3, base_idx + 3] = 0.0
        # Point 3: Top-Left
        points[1, base_idx + 4] = b.x1; points[2, base_idx + 4] = b.y2; points[3, base_idx + 4] = 0.0
        
        # 0-based indexing for XDMF
        cells[1, i] = base_idx
        cells[2, i] = base_idx + 1
        cells[3, i] = base_idx + 2
        cells[4, i] = base_idx + 3
    end

    # 3. Write the HDF5 File
    println("Writing HDF5 to $h5_filename...")
    h5open(h5_filename, "w") do file
        # Write your exact requested compound dataset
        # HDF5.jl automatically maps Julia arrays of structs to H5T_COMPOUND
        write(file, "boxes", boxes)
        
        # Write the ParaView required datasets
        write(file, "Points", points)
        write(file, "Cells", cells)
    end

    # 4. Write the XDMF File
    println("Writing XDMF to $xmf_filename...")
    # Get just the filename without the path for the XML reference
    h5_basename = basename(h5_filename)
    
    open(xmf_filename, "w") do io
        write(io, """
        <?xml version="1.0" ?>
        <!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>
        <Xdmf Version="2.0">
          <Domain>
            <Grid Name="VLSI_Layout" GridType="Uniform">
              <Topology TopologyType="Quadrilateral" NumberOfElements="$num_boxes">
                <DataItem Dimensions="$num_boxes 4" NumberType="Int" Format="HDF">
                  $h5_basename:/Cells
                </DataItem>
              </Topology>
              <Geometry GeometryType="XYZ">
                <DataItem Dimensions="$(num_boxes * 4) 3" NumberType="Float" Format="HDF">
                  $h5_basename:/Points
                </DataItem>
              </Geometry>
            </Grid>
          </Domain>
        </Xdmf>
        """)
    end

    println("Done! You can now load $xmf_filename in ParaView.")
end

# Usage:
# convert_layout_to_hdf5("layout.txt", "SEDFXTP1_L11.h5", "SEDFXTP1_L11.xmf")


# 1. Upgraded schema to store layer and datatype
struct BoxData
    x1::Int32
    y1::Int32
    x2::Int32
    y2::Int32
    layer::Int32
    datatype::Int32
end

function convert_layout_to_hdf5(input_txt::String, h5_filename::String, xmf_filename::String)
    boxes = BoxData[]

    println("Parsing layout file for multi-layer data...")
    for line in eachline(input_txt)
        line = strip(line)
        
        if startswith(line, "box")
            # Upgraded Regex: Captures layer (1), datatype (2), pt1 (3), pt2 (4)
            m = match(r"box\s+(\d+)\s+(\d+)\s+.*?\{([^}]+)\}\s*\{([^}]+)\}", line)
            
            if m !== nothing
                layer    = parse(Int32, m[1])
                datatype = parse(Int32, m[2])
                pt1      = parse.(Int32, split(strip(m[3])))
                pt2      = parse.(Int32, split(strip(m[4])))
                
                x1, x2 = min(pt1[1], pt2[1]), max(pt1[1], pt2[1])
                y1, y2 = min(pt1[2], pt2[2]), max(pt1[2], pt2[2])
                
                push!(boxes, BoxData(x1, y1, x2, y2, layer, datatype))
            end
            
        elseif startswith(line, "boundary")
            # Upgraded Regex: Captures layer (1), datatype (2), and the rest of the string (3)
            m = match(r"boundary\s+(\d+)\s+(\d+)\s+(.*)", line)
            
            if m !== nothing
                layer      = parse(Int32, m[1])
                datatype   = parse(Int32, m[2])
                coords_str = m[3]

                xs = Int[]
                ys = Int[]
                for c in eachmatch(r"\{([^}]+)\}", coords_str)
                    coords = parse.(Int, split(strip(c[1])))
                    push!(xs, coords[1])
                    push!(ys, coords[2])
                end
                
                if length(xs) > 0
                    fractured_rects = fracture_to_rects(xs, ys)
                    for r in fractured_rects
                        push!(boxes, BoxData(Int32(r[1]), Int32(r[2]), Int32(r[3]), Int32(r[4]), layer, datatype))
                    end
                end
            end
        end
    end

    num_boxes = length(boxes)
    println("Successfully parsed $num_boxes rectangles across multiple layers.")

    if num_boxes == 0
        println("ERROR: No boxes found. Aborting.")
        return
    end

    # 2. Generate ParaView Mesh & Attribute Data
    points = zeros(Float32, 3, num_boxes * 4) 
    cells  = zeros(Int32, 4, num_boxes)       
    
    # NEW: Isolate the layer data so ParaView can read it directly as a Cell Attribute
    layer_array = zeros(Int32, num_boxes)

    for i in 1:num_boxes
        b = boxes[i]
        base_idx = (i - 1) * 4
        
        points[1, base_idx + 1] = b.x1; points[2, base_idx + 1] = b.y1; points[3, base_idx + 1] = 0.0
        points[1, base_idx + 2] = b.x2; points[2, base_idx + 2] = b.y1; points[3, base_idx + 2] = 0.0
        points[1, base_idx + 3] = b.x2; points[2, base_idx + 3] = b.y2; points[3, base_idx + 3] = 0.0
        points[1, base_idx + 4] = b.x1; points[2, base_idx + 4] = b.y2; points[3, base_idx + 4] = 0.0
        
        cells[1, i] = base_idx
        cells[2, i] = base_idx + 1
        cells[3, i] = base_idx + 2
        cells[4, i] = base_idx + 3
        
        # Populate the attribute array
        layer_array[i] = b.layer
    end

    # 3. Write the HDF5 File
    println("Writing HDF5 to $h5_filename...")
    h5open(h5_filename, "w") do file
        write(file, "boxes", boxes) # Your programmatic schema
        write(file, "Points", points)
        write(file, "Cells", cells)
        write(file, "Layers", layer_array) # NEW: ParaView attribute dataset
    end

    # 4. Write the XDMF File
    println("Writing XDMF to $xmf_filename...")
    h5_basename = basename(h5_filename)
    
    open(xmf_filename, "w") do io
        write(io, """
        <?xml version="1.0" ?>
        <!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>
        <Xdmf Version="2.0">
          <Domain>
            <Grid Name="VLSI_Layout" GridType="Uniform">
              <Topology TopologyType="Quadrilateral" NumberOfElements="$num_boxes">
                <DataItem Dimensions="$num_boxes 4" NumberType="Int" Format="HDF">
                  $h5_basename:/Cells
                </DataItem>
              </Topology>
              <Geometry GeometryType="XYZ">
                <DataItem Dimensions="$(num_boxes * 4) 3" NumberType="Float" Format="HDF">
                  $h5_basename:/Points
                </DataItem>
              </Geometry>
              <Attribute Name="Layer" AttributeType="Scalar" Center="Cell">
                <DataItem Dimensions="$num_boxes" NumberType="Int" Format="HDF">
                  $h5_basename:/Layers
                </DataItem>
              </Attribute>
            </Grid>
          </Domain>
        </Xdmf>
        """)
    end
end

#=
For mkFPU_FLAT, these are the ONLY layers
64 20 1 100
65 20 2 200
66 20 3 300
67 20 4 400
68 20 5 500
69 20 6 600
70 20 7 700
71 20 8 800
72 20 9 900
235 4 10 50
=#
# The keys are Tuples: (Val1, Val2)
# The values are NamedTuples for readable access: (file_num=X, z_val=Y)


# 1. Define the global mapping dictionary
const MAPPING_DICT_FPU = Dict(
    (64, 20)  => (file_num = 1,  z_val = 100.0f0),
    (65, 20)  => (file_num = 2,  z_val = 200.0f0),
    (66, 20)  => (file_num = 3,  z_val = 300.0f0),
    (67, 20)  => (file_num = 4,  z_val = 400.0f0),
    (68, 20)  => (file_num = 5,  z_val = 500.0f0),
    (69, 20)  => (file_num = 6,  z_val = 600.0f0),
    (70, 20)  => (file_num = 7,  z_val = 700.0f0),
    (71, 20)  => (file_num = 8,  z_val = 800.0f0),
    (72, 20)  => (file_num = 9,  z_val = 900.0f0),
    (235, 4)  => (file_num = 10, z_val = 50.0f0)
)

#cat /scratch1/skoranne/OSS_EDA_TOOLS/DESIGNS/MW_FLAT_DATA/UNIQ_LD.txt |awk '{printf "("$2","$3│········
#") => " "(file_num = " NR ", z_val = " NR*100  "),\n"}'
const MAPPING_DICT = Dict(
(64,20) => (file_num = 1, z_val = 100),
(65,20) => (file_num = 2, z_val = 200),
(66,20) => (file_num = 3, z_val = 300),
(67,20) => (file_num = 4, z_val = 400),
(68,20) => (file_num = 5, z_val = 500),
(93,44) => (file_num = 6, z_val = 600),
(94,20) => (file_num = 7, z_val = 700),
(95,20) => (file_num = 8, z_val = 800),
(122,16) => (file_num = 9, z_val = 900),
(235,4) => (file_num = 10, z_val = 1000),
(236,0) => (file_num = 11, z_val = 1100),
(64,16) => (file_num = 12, z_val = 1200),
(64,20) => (file_num = 13, z_val = 1300),
(65,20) => (file_num = 14, z_val = 1400),
(65,44) => (file_num = 15, z_val = 1500),
(66,15) => (file_num = 16, z_val = 1600),
(66,20) => (file_num = 17, z_val = 1700),
(66,44) => (file_num = 18, z_val = 1800),
(67,16) => (file_num = 19, z_val = 1900),
(67,20) => (file_num = 20, z_val = 2000),
(67,44) => (file_num = 21, z_val = 2100),
(68,16) => (file_num = 22, z_val = 2200),
(68,20) => (file_num = 23, z_val = 2300),
(68,44) => (file_num = 24, z_val = 2400),
(69,16) => (file_num = 25, z_val = 2500),
(69,20) => (file_num = 26, z_val = 2600),
(69,44) => (file_num = 27, z_val = 2700),
(70,16) => (file_num = 28, z_val = 2800),
(70,20) => (file_num = 29, z_val = 2900),
(70,44) => (file_num = 30, z_val = 3000),
(71,16) => (file_num = 31, z_val = 3100),
(71,20) => (file_num = 32, z_val = 3200),
(71,44) => (file_num = 33, z_val = 3300),
(72,16) => (file_num = 34, z_val = 3400),
(72,20) => (file_num = 35, z_val = 3500),
(78,44) => (file_num = 36, z_val = 3600),
(81,14) => (file_num = 37, z_val = 3700),
(81,23) => (file_num = 38, z_val = 3800),
(81,4) => (file_num = 39, z_val = 3900),
(93,44) => (file_num = 40, z_val = 4000),
(94,20) => (file_num = 41, z_val = 4100),
(95,20) => (file_num = 42, z_val = 4200)
)

using TranscodingStreams
function convert_layout_to_hdf5(input_file::String, base_h5_filename::String)
    # Group the parsed boxes by their target file number
    # Key: file_num, Value: Vector of BoxData
    layer_groups = Dict{Int, Vector{BoxData}}()

    println("Parsing layout file for multi-layer data...")
    open(input_file) do file
        decompressor_stream = ZstdDecompressorStream(file)
        
    for line in eachline(decompressor_stream)
        line = strip(line)
        
        if startswith(line, "box")
            m = match(r"box\s+(\d+)\s+(\d+)\s+.*?\{([^}]+)\}\s*\{([^}]+)\}", line)
            
            if m !== nothing
                layer    = parse(Int32, m[1])
                datatype = parse(Int32, m[2])
                pt1      = parse.(Int32, split(strip(m[3])))
                pt2      = parse.(Int32, split(strip(m[4])))
                
                x1, x2 = min(pt1[1], pt2[1]), max(pt1[1], pt2[1])
                y1, y2 = min(pt1[2], pt2[2]), max(pt1[2], pt2[2])
                
                key = (Int(layer), Int(datatype))
                if haskey(MAPPING_DICT, key)
                    file_num = MAPPING_DICT[key].file_num
                    box = BoxData(x1, y1, x2, y2, layer, datatype)
                    push!(get!(() -> BoxData[], layer_groups, file_num), box)
                else
                    println("⚠️ Skipping box: No dictionary entry for key combo $key")
                end
            end
            
        elseif startswith(line, "boundary")
            #m = match(r"boundary\s+(\d+)\s+(\d+)\s+(.*)", line)
            m = match(r"box\s+.*?\{([^}]+)\}\s*\{([^}]+)\}", line)

            if m !== nothing
                layer      = parse(Int32, m[1])
                datatype   = parse(Int32, m[2])
                coords_str = m[3]

                xs = Int[]
                ys = Int[]
                for c in eachmatch(r"\{([^}]+)\}", coords_str)
                    coords = parse.(Int, split(strip(c[1])))
                    push!(xs, coords[1])
                    push!(ys, coords[2])
                end
                
                if length(xs) > 0
                    key = (Int(layer), Int(datatype))
                    if haskey(MAPPING_DICT, key)
                        file_num = MAPPING_DICT[key].file_num
                        fractured_rects = fracture_to_rects(xs, ys)
                        
                        box_list = get!(() -> BoxData[], layer_groups, file_num)
                        for r in fractured_rects
                            push!(box_list, BoxData(Int32(r[1]), Int32(r[2]), Int32(r[3]), Int32(r[4]), layer, datatype))
                        end
                    else
                        println("⚠️ Skipping boundary: No dictionary entry for key combo $key")
                    end
                end
            end
        end
    end
    end
    
    if isempty(layer_groups)
        println("ERROR: No valid boxes mapped to dictionary keys. Aborting.")
        return
    end

    # 2. Process and save each layer's specific file group
    for (file_num, boxes) in layer_groups
        num_boxes = length(boxes)
        
        # Get the global Z coordinate for this specific file layer group
        # We can find this by querying the first item since they all share a file map key
        sample_key = (Int(boxes[1].layer), Int(boxes[1].datatype))
        z_val = MAPPING_DICT[sample_key].z_val

        # Create output filenames: e.g., "SEDFXTP1_DATA_L3.h5"
        base_name, ext = splitext(base_h5_filename)
        #h5_filename   = "$(base_name)_L$(file_num)$(ext)"
        h5_filename   = "$(base_name)_L$(boxes[1].layer)_D$(boxes[1].datatype)$(ext)"
        #xmf_filename  = "$(base_name)_L$(file_num).xmf"
        xmf_filename  = "$(base_name)_L$(boxes[1].layer)_D$(boxes[1].datatype).xmf"

        println("Processing Layer File: $h5_filename with $num_boxes boxes at Z = $z_val")

        # Generate ParaView Mesh Grid Data
        points = zeros(Float32, 3, num_boxes * 4) 
        cells  = zeros(Int32, 4, num_boxes)       
        layer_array = zeros(Int32, num_boxes)

        for i in 1:num_boxes
            b = boxes[i]
            base_idx = (i - 1) * 4
            
            # Map coordinates and assign the true Z value dynamically
            points[1, base_idx + 1] = b.x1; points[2, base_idx + 1] = b.y1; points[3, base_idx + 1] = z_val
            points[1, base_idx + 2] = b.x2; points[2, base_idx + 2] = b.y1; points[3, base_idx + 2] = z_val
            points[1, base_idx + 3] = b.x2; points[2, base_idx + 3] = b.y2; points[3, base_idx + 3] = z_val
            points[1, base_idx + 4] = b.x1; points[2, base_idx + 4] = b.y2; points[3, base_idx + 4] = z_val
            
            cells[1, i] = base_idx
            cells[2, i] = base_idx + 1
            cells[3, i] = base_idx + 2
            cells[4, i] = base_idx + 3
            
            layer_array[i] = b.layer
        end

        # Write the specialized layer HDF5 File
        h5open(h5_filename, "w") do file
            write(file, "boxes", boxes) 
            write(file, "Points", points)
            write(file, "Cells", cells)
            write(file, "Layers", layer_array) 
        end
        
        # Proactively generate a matching XDMF file companion for ParaView
        write_xdmf_companion(xmf_filename, basename(h5_filename), num_boxes)
    end
end

# Helper function to auto-generate the XDMF files for ParaView
function write_xdmf_companion(xmf_path::String, h5_basename::String, num_boxes::Int)
    open(xmf_path, "w") do io
        write(io, """<?xml version="1.0" ?>
<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>
<Xdmf Version="2.0">
  <Domain>
    <Grid Name="LayerGrid" GridType="Uniform">
      <Topology TopologyType="Polygon" NumberOfElements="$num_boxes">
        <DataItem Dimensions="$num_boxes 4" NumberType="Int" Format="HDF">
          $h5_basename:/Cells
        </DataItem>
      </Topology>
      <Geometry GeometryType="XYZ">
        <DataItem Dimensions="$(num_boxes * 4) 3" NumberType="Float" Format="HDF">
          $h5_basename:/Points
        </DataItem>
      </Geometry>
      <Attribute Name="LayerID" AttributeType="Scalar" Center="Cell">
        <DataItem Dimensions="$num_boxes" NumberType="Int" Format="HDF">
          $h5_basename:/Layers
        </DataItem>
      </Attribute>
    </Grid>
  </Domain>
</Xdmf>
""")
    end
end
