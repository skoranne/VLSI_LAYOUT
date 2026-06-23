# Setup the layout object
layout = RBA::Layout.new
layout.read($input)

# Configure the OASIS save options
options = RBA::SaveLayoutOptions.new

# 1. Force the output format to OASIS
options.format = "OASIS"

# 2. Turn on CBLOCK compression
options.oasis_write_cblocks = true

# 3. Turn on REPETITION to shrink file size
options.oasis_compression_level = 10

# Save the file with the new options
layout.write($output, options)
