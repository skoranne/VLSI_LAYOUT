# Minimal Julia ccall test for layoutdb C API
const lib = joinpath(@__DIR__, "liblayoutdb.so") # if built as shared; else use executables
println("See db_extent etc executables for CLI tests. For ccall, build shared lib: g++ -shared -fPIC -o liblayoutdb.so layoutdb.cpp gdsii.cpp oasis.cpp")
