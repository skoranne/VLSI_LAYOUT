################################################################################
# File   : checkplot.jl
# Author : Sandeep Koranne (C) 2026
# Purpose: Julia plotting
################################################################################
using Plots
using SpatialIndexing


function plot_spatial_indexing_tree(filename::String;k::Int=16)
    # 1. Read the boxes from your file
    boxes = BBox[]
    for line in eachline(filename)
        m = match(r"Box\s+(\d+):\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)", line)
        if m !== nothing
            push!(boxes, BBox(
                parse(Int, m.captures[1]),
                parse(Float64, m.captures[2]),
                parse(Float64, m.captures[3]),
                parse(Float64, m.captures[4]),
                parse(Float64, m.captures[5])
            ))
        end
    end

    # 2. Initialize the SpatialIndexing RTree
    # We specify Float64 for coordinates, 2 dimensions, Int for IDs, and Nothing for values
    tree = RTree{Float64, 2}(Int, Nothing; leaf_capacity=k, branch_capacity=k)
    
    # 3. Insert all bounding boxes into the tree
    for box in boxes
        # SpatialIndexing.jl uses Rect((min_x, min_y), (max_x, max_y))
        rect = SpatialIndexing.Rect((box.xmin, box.ymin), (box.xmax, box.ymax))
        insert!(tree, rect, box.id, nothing)
    end
# 4. Helper function to recursively traverse the tree and extract MBRs by level
    function extract_levels!(node, level_dict)
        # Branch nodes have a level; Leaf nodes do not (they are implicitly level 1)
        lvl = hasproperty(node, :level) ? node.level : 1
        
        if !haskey(level_dict, lvl)
            level_dict[lvl] = []
        end
        
        # Store the MBR for this specific node
        push!(level_dict[lvl], node.mbr)
        
        # Only recurse if it's a branch node (level > 1). 
        # A Leaf's 'children' are the data items, not sub-nodes.
        if lvl > 1 && hasproperty(node, :children)
            for child in node.children
                extract_levels!(child, level_dict)
            end
        end
        return level_dict
    end    
    
    # Extract levels starting from the root
    levels_map = extract_levels!(tree.root, Dict{Int, Vector{Any}}())
    
    # 5. Plot the tree levels
    p = plot(legend=:outertopright, aspect_ratio=:equal, title="SpatialIndexing.jl RTree Levels", yflip=true)
    
    # Sort levels descending so we plot the highest level (Root) first, then work down to the leaves
    sorted_levels = sort(collect(keys(levels_map)), rev=true)
    
    # Color palette to visually distinguish the hierarchy
    colors = [:red, :blue, :green, :purple, :orange]
    
    for (i, lvl) in enumerate(sorted_levels)
        mbrs = levels_map[lvl]
        c = colors[mod1(i, length(colors))] # Cycle through colors safely
        
        for (j, mbr) in enumerate(mbrs)
            # SpatialIndexing Rects store coordinates in `low` and `high` tuples
            min_x, min_y = mbr.low
            max_x, max_y = mbr.high
            
            rect_shape = Shape([
                (min_x, min_y),
                (max_x, min_y),
                (max_x, max_y),
                (min_x, max_y)
            ])
            
            # Only add a legend label for the very first rectangle in each level to avoid clutter
            lbl = j == 1 ? "Level $lvl (Nodes: $(length(mbrs)))" : ""
            
            # Make the higher-level MBRs thicker so they stand out
            lw = 1.0 + (lvl * 0.7)
            
            plot!(p, rect_shape, fillalpha=0.0, linecolor=c, linewidth=lw, label=lbl)
        end
    end
    
    display(p)
    return tree
end

# Usage:
# plot_spatial_indexing_tree("boxes.txt")


# Define a simple struct to hold our box data for easy grouping
struct BBox
    id::Int
    xmin::Float64
    ymin::Float64
    xmax::Float64
    ymax::Float64
end

function plot_rtree_mbrs(filename::String; k::Int=32)
    # Initialize the plot
    p = plot(legend=false, aspect_ratio=:equal, title="RTree MBRs (k=$k)", yflip=true)
    
    boxes = BBox[]
    
    # 1. Read the file and store all boxes
    for line in eachline(filename)
        m = match(r"Box\s+(\d+):\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)", line)
        if m !== nothing
            id = parse(Int, m.captures[1])
            xmin = parse(Float64, m.captures[2])
            ymin = parse(Float64, m.captures[3])
            xmax = parse(Float64, m.captures[4])
            ymax = parse(Float64, m.captures[5])
            push!(boxes, BBox(id, xmin, ymin, xmax, ymax))
        end
    end
    
    # 2. Plot all individual boxes first (so they are in the background)
    for box in boxes
        rect = Shape([
            (box.xmin, box.ymin),
            (box.xmax, box.ymin),
            (box.xmax, box.ymax),
            (box.xmin, box.ymax)
        ])
        
        # Lighter colors for individual boxes
        plot!(p, rect, fillalpha=0.2, linecolor=:gray, seriescolor=:lightblue)
        
        xc = (box.xmin + box.xmax) / 2.0
        yc = (box.ymin + box.ymax) / 2.0
        annotate!(p, xc, yc, text(string(box.id), 3, :gray, :center))
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
        annotate!(p, mbr_xc, mbr_ymin - 5, text("MBR $group_num", 9, :red, :bottom))
    end
    
    # Show the plot
    display(p)
    return p
end

# Usage:
# Assuming your data is saved in "boxes.txt", grouping by 32
# plot_rtree_mbrs("boxes.txt", k=32)

# If you want to test it with a smaller group size, e.g., 2 boxes per MBR:
# plot_rtree_mbrs("boxes.txt", k=2)


function plot_boxes(filename::String)
    # Initialize an empty plot with equal aspect ratio
    p = plot(legend=false, aspect_ratio=:equal, title="Bounding Boxes", yflip=true)
    
    # Read the file line by line
    for line in eachline(filename)
        # Regular expression to match: Box [id]: [xmin] [ymin] [xmax] [ymax]
        m = match(r"Box\s+(\d+):\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)", line)
        
        if m !== nothing
            # Parse the matched strings into integers
            id = parse(Int, m.captures[1])
            xmin = parse(Float64, m.captures[2])
            ymin = parse(Float64, m.captures[3])
            xmax = parse(Float64, m.captures[4])
            ymax = parse(Float64, m.captures[5])
            
            # Calculate the center of the rectangle for the text annotation
            x_center = (xmin + xmax) / 2.0
            y_center = (ymin + ymax) / 2.0
            
            # Create a Shape for the rectangle
            rect = Shape([
                (xmin, ymin),
                (xmax, ymin),
                (xmax, ymax),
                (xmin, ymax)
            ])
            
            # Plot the rectangle onto the existing plot 'p'
            plot!(p, rect, fillalpha=0.3, linecolor=:black, seriescolor=:lightblue)
            
            # Add the Box ID at the center
            annotate!(p, x_center, y_center, text(string(id), 4, :black, :center))
        end
    end
    
    # Show the plot
    display(p)
    return p
end

# Usage:
# Assuming your data is saved in "boxes.txt"
# plot_boxes("boxes.txt")
