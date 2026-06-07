using HDF5
using DataFrames

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
