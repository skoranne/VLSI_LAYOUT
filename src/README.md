              VLSI Layout Descriptor

  This file contains some example programs I have for
sequential logic synthesis, BLIF processing, and gate
level Verilog analysis.

  I am adding to this Magic VLSI layout reader which
was written in 1995-1996, and modified for modern C++
and boost::spatial_index which can be tried.

grep  "<<" FPU_FLAT.mag | awk '{print $0; print "HDF5 FPU_DATA/FPU_FLAT_L" ++count ".h5"}'


 Reading filename: SDT6x6_FLAT.mag.gz
 The file is a gzipped file!
 MAGIC is using scaling parameters:            1             2
  Layer = nwell         = id:     1
  Layer = pwell         = id:     2
  Layer = nmos          = id:     3
  Layer = pmos          = id:     4
  Layer = ndiff         = id:     5
  Layer = pdiff         = id:     6
  Layer = ndiffc        = id:     7
  Layer = pdiffc        = id:     8
  Layer = poly          = id:     9
  Layer = polycont      = id:    10
  Layer = locali        = id:    11
  Layer = viali         = id:    12
  Layer = metal1        = id:    13
  Layer = via1          = id:    14
  Layer = metal2        = id:    15
  Layer = via2          = id:    16
  Layer = metal3        = id:    17
  Layer = via3          = id:    18
  Layer = metal4        = id:    19
  Layer = via4          = id:    20
  Layer = metal5        = id:    21
 Parsed           21  layers.
Sorting completed in 134.19 seconds.
 === number of boxes stored per layer ===
 Layer:            1  nwell                has                   6948  rects.
 Layer:            2  pwell                has                3032856  rects.
 Layer:            3  nmos                 has               16647768  rects.
 Layer:            4  pmos                 has               16647768  rects.
 Layer:            5  ndiff                has              110255004  rects.
 Layer:            6  pdiff                has              155351196  rects.
 Layer:            7  ndiffc               has               28093212  rects.
 Layer:            8  pdiffc               has               43431120  rects.
 Layer:            9  poly                 has              149576256  rects.
 Layer:           10  polycont             has               18863568  rects.
 Layer:           11  locali               has              310901868  rects.
 Layer:           12  viali                has               47874888  rects.
 Layer:           13  metal1               has               87869844  rects.
 Layer:           14  via1                 has               16040772  rects.
 Layer:           15  metal2               has               18663972  rects.
 Layer:           16  via2                 has                2222208  rects.
 Layer:           17  metal3               has                2193966  rects.
 Layer:           18  via3                 has                2045880  rects.
 Layer:           19  metal4               has                 805104  rects.
 Layer:           20  via4                 has                  70956  rects.
 Layer:           21  metal5               has                  45720  rects.
 +-------------------------- Design Extent ------------------------+
 Box: [           0 ,           0 ] to [     1272639 ,     1284783 ]
 +-----------------------------------------------------------------+
 Box: [        1066 ,        2437 ] to [     1271546 ,     1281899 ]
 Box: [        1105 ,        2189 ] to [     1271507 ,     1282147 ]
 Box: [        1183 ,        2223 ] to [     1271429 ,     1282113 ]
 Box: [        1183 ,        2473 ] to [     1271429 ,     1281863 ]
 Box: [        1131 ,        2215 ] to [     1271481 ,     1282121 ]
 Box: [        1131 ,        2473 ] to [     1271481 ,     1281863 ]
 Box: [        1139 ,        2233 ] to [     1271473 ,     1282103 ]
 Box: [        1139 ,        2483 ] to [     1271473 ,     1281853 ]
 Box: [        1155 ,        2197 ] to [     1271457 ,     1282139 ]
 Box: [        1171 ,        2333 ] to [     1271441 ,     1282003 ]
 Box: [        1104 ,        2159 ] to [     1271508 ,     1282177 ]
 Box: [        1133 ,        2159 ] to [     1271479 ,     1282177 ]
 Box: [         106 ,          76 ] to [     1272414 ,     1284736 ]
 Box: [         112 ,          76 ] to [     1272408 ,     1284736 ]
 Box: [         110 ,           0 ] to [     1272410 ,     1284783 ]
 Box: [         110 ,        1264 ] to [     1272410 ,     1283344 ]
 Box: [           0 ,        1259 ] to [     1272639 ,     1283349 ]
 Box: [         980 ,        2144 ] to [     1270068 ,     1282668 ]
 Box: [         979 ,        2128 ] to [     1270069 ,     1282669 ]
 Box: [        2366 ,        5388 ] to [     1268682 ,     1281938 ]
 Box: [        1104 ,        5346 ] to [     1271508 ,     1281980 ]
 
/ (Root)
├── Layer_01/ (Group)
│   ├── [Attributes] 
│   │   ├── LayerName: "Metal_1" (String)
│   │   └── BoundingBox: [0.0, 0.0, 5000.0, 5000.0] (Array of Reals/Doubles)
│   └── Boxes (Dataset of Compound Datatypes)
│
├── Layer_02/ (Group)
│   ├── [Attributes]
│   │   ├── LayerName: "Via_1" 
│   │   └── BoundingBox: [10.0, 10.0, 4990.0, 4990.0]
│   └── Boxes (Dataset of Compound Datatypes)
...
└── Layer_30/ (Group)
