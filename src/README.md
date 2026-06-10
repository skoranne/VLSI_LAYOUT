              VLSI Layout Descriptor

  This file contains some example programs I have for
sequential logic synthesis, BLIF processing, and gate
level Verilog analysis.

  I am adding to this Magic VLSI layout reader which
was written in 1995-1996, and modified for modern C++
and boost::spatial_index which can be tried.

grep  "<<" FPU_FLAT.mag | awk '{print $0; print "HDF5 FPU_DATA/FPU_FLAT_L" ++count ".h5"}'
Also we can use 'ratarmount' if we dont want to support .gz ourselves.

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

TODO:
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
 Reading filename: MY_SEDFXTP1.mag
 The file is NOT a gzipped file, w/prefix: MY_SEDFXTP1
 MAGIC is using scaling parameters:            1             2
  Layer = nwell         = id:     1
 NUM BOXES in DATA FILE:                      3
RL: 1    from HDF5: SEDFXTP1_DATA/S               3
  Layer = pwell         = id:     2
 NUM BOXES in DATA FILE:                      7
RL: 2    from HDF5: SEDFXTP1_DATA/S               7
  Layer = scnmos        = id:     3
 NUM BOXES in DATA FILE:                     21
RL: 3    from HDF5: SEDFXTP1_DATA/S               21
  Layer = scpmoshvt     = id:     4
 NUM BOXES in DATA FILE:                     21
RL: 4    from HDF5: SEDFXTP1_DATA/S               21
  Layer = ndiff         = id:     5
 NUM BOXES in DATA FILE:                    108
RL: 5    from HDF5: SEDFXTP1_DATA/S               108
  Layer = pdiff         = id:     6
 NUM BOXES in DATA FILE:                    144
RL: 6    from HDF5: SEDFXTP1_DATA/S               144
  Layer = ndiffc        = id:     7
 NUM BOXES in DATA FILE:                     25
RL: 7    from HDF5: SEDFXTP1_DATA/S               25
  Layer = pdiffc        = id:     8
 NUM BOXES in DATA FILE:                     38
RL: 8    from HDF5: SEDFXTP1_DATA/S               38
  Layer = poly          = id:     9
 NUM BOXES in DATA FILE:                    197
RL: 9    from HDF5: SEDFXTP1_DATA/S               197
  Layer = polycont      = id:    10
 NUM BOXES in DATA FILE:                     23
RL: 10    from HDF5: SEDFXTP1_DATA/S               23
  Layer = locali        = id:    11
 NUM BOXES in DATA FILE:                    391
RL: 11    from HDF5: SEDFXTP1_DATA/S               391
  Layer = viali         = id:    12
 NUM BOXES in DATA FILE:                     76
RL: 12    from HDF5: SEDFXTP1_DATA/S               76
  Layer = metal1        = id:    13
 NUM BOXES in DATA FILE:                    127
RL: 13    from HDF5: SEDFXTP1_DATA/S               127
 Parsed           13  layers.
Sorting/OMT completed in         0.60 seconds.
 === number of boxes stored per layer ===
Layer:   1 nwell    has            3 rects. |RTREE| =         0.00 secs.
Layer:   2 pwell    has            7 rects. |RTREE| =         0.00 secs.
Layer:   3 scnmos   has           21 rects. |RTREE| =         0.00 secs.
Layer:   4 scpmoshv has           21 rects. |RTREE| =         0.00 secs.
Layer:   5 ndiff    has          108 rects. |RTREE| =         0.00 secs.
Layer:   6 pdiff    has          144 rects. |RTREE| =         0.00 secs.
Layer:   7 ndiffc   has           25 rects. |RTREE| =         0.00 secs.
Layer:   8 pdiffc   has           38 rects. |RTREE| =         0.00 secs.
Layer:   9 poly     has          197 rects. |RTREE| =         0.00 secs.
Layer:  10 polycont has           23 rects. |RTREE| =         0.00 secs.
Layer:  11 locali   has          391 rects. |RTREE| =         0.02 secs.
Layer:  12 viali    has           76 rects. |RTREE| =         0.00 secs.
Layer:  13 metal1   has          127 rects. |RTREE| =         0.00 secs.
 +-------------------------- Design Extent ------------------------+
 Box: [         -38 ,         -48 ] to [        2706 ,         592 ]
 +-----------------------------------------------------------------+
 Box: [         -38 ,         261 ] to [        2706 ,         582 ]
 Box: [           1 ,         -17 ] to [        2639 ,         229 ]
 Box: [          79 ,          47 ] to [        2557 ,         203 ]
 Box: [          79 ,         297 ] to [        2557 ,         497 ]
 Box: [          27 ,          47 ] to [        2613 ,         203 ]
 Box: [          27 ,         297 ] to [        2613 ,         497 ]
 Box: [          35 ,          59 ] to [        2603 ,         180 ]
 Box: [          35 ,         309 ] to [        2603 ,         485 ]
 Box: [          23 ,          21 ] to [        2557 ,         523 ]
 Box: [          33 ,          51 ] to [        2362 ,         365 ]
 Box: [           0 ,         -17 ] to [        2668 ,         561 ]
 Box: [          29 ,         -17 ] to [        2639 ,         561 ]
 Box: [           0 ,         -48 ] to [        2668 ,         592 ]
 +-----------------------------------------------------------------+
 
 Reading filename: MY_FPU_FLAT.mag
 The file is NOT a gzipped file, w/prefix: MY_FPU_FLAT
 MAGIC is using scaling parameters:            1             2
  Layer = nwell         = id:     1
 NUM BOXES in DATA FILE:                    328
RL: 1    from HDF5: /scratch1/skora               328
  Layer = pwell         = id:     2
 NUM BOXES in DATA FILE:                1444879
RL: 2    from HDF5: /scratch1/skora               1444879
  Layer = scnmos        = id:     3
 NUM BOXES in DATA FILE:                1110963
RL: 3    from HDF5: /scratch1/skora               1110963
  Layer = scpmoshvt     = id:     4
 NUM BOXES in DATA FILE:                1110963
RL: 4    from HDF5: /scratch1/skora               1110963
  Layer = ndiff         = id:     5
 NUM BOXES in DATA FILE:                7698513
RL: 5    from HDF5: /scratch1/skora               7698513
  Layer = pdiff         = id:     6
 NUM BOXES in DATA FILE:               12153635
RL: 6    from HDF5: /scratch1/skora               12153635
  Layer = ndiffc        = id:     7
 NUM BOXES in DATA FILE:                1951634
RL: 7    from HDF5: /scratch1/skora               1951634
  Layer = pdiffc        = id:     8
 NUM BOXES in DATA FILE:                3439988
RL: 8    from HDF5: /scratch1/skora               3439988
  Layer = psubdiff      = id:     9
 NUM BOXES in DATA FILE:                  90804
RL: 9    from HDF5: /scratch1/skora               90804
  Layer = nsubdiff      = id:    10
 NUM BOXES in DATA FILE:                 136206
RL: 10    from HDF5: /scratch1/skora               136206
  Layer = psubdiffco    = id:    11
 NUM BOXES in DATA FILE:                  45402
RL: 11    from HDF5: /scratch1/skora               45402
  Layer = nsubdiffco    = id:    12
 NUM BOXES in DATA FILE:                  90804
RL: 12    from HDF5: /scratch1/skora               90804
  Layer = poly          = id:    13
 NUM BOXES in DATA FILE:               11573460
RL: 13    from HDF5: /scratch1/skora               11573460
  Layer = polycont      = id:    14
 NUM BOXES in DATA FILE:                1535008
RL: 14    from HDF5: /scratch1/skora               1535008
  Layer = rmp           = id:    15
 NUM BOXES in DATA FILE:                      2
RL: 15    from HDF5: /scratch1/skora               2
  Layer = ndiode        = id:    16
 NUM BOXES in DATA FILE:                   9459
RL: 16    from HDF5: /scratch1/skora               9459
  Layer = ndiodec       = id:    17
 NUM BOXES in DATA FILE:                   4204
RL: 17    from HDF5: /scratch1/skora               4204
  Layer = locali        = id:    18
 NUM BOXES in DATA FILE:               22787594
RL: 18    from HDF5: /scratch1/skora               22787594
  Layer = viali         = id:    19
 NUM BOXES in DATA FILE:                3318330
RL: 19    from HDF5: /scratch1/skora               3318330
  Layer = metal1        = id:    20
 NUM BOXES in DATA FILE:                5365556
RL: 20    from HDF5: /scratch1/skora               5365556
  Layer = via1          = id:    21
 NUM BOXES in DATA FILE:                1026776
RL: 21    from HDF5: /scratch1/skora               1026776
  Layer = metal2        = id:    22
 NUM BOXES in DATA FILE:                1197256
RL: 22    from HDF5: /scratch1/skora               1197256
  Layer = via2          = id:    23
 NUM BOXES in DATA FILE:                 170296
RL: 23    from HDF5: /scratch1/skora               170296
  Layer = metal3        = id:    24
 NUM BOXES in DATA FILE:                 139228
RL: 24    from HDF5: /scratch1/skora               139228
  Layer = via3          = id:    25
 NUM BOXES in DATA FILE:                 164730
RL: 25    from HDF5: /scratch1/skora               164730
  Layer = metal4        = id:    26
 NUM BOXES in DATA FILE:                  64915
RL: 26    from HDF5: /scratch1/skora               64915
  Layer = via4          = id:    27
 NUM BOXES in DATA FILE:                   2120
RL: 27    from HDF5: /scratch1/skora               2120
  Layer = metal5        = id:    28
 NUM BOXES in DATA FILE:                   1230
RL: 28    from HDF5: /scratch1/skora               1230
 Parsed           28  layers.
Sorting/OMT completed in         7.40 CPU seconds.        0.33 REAL seconds.
 === number of boxes stored per layer ===
 +-----------------------------------------------------------------+
Layer:   1 nwell    has          328 rects. |RTREE| = CPU         0.01 secs.        0.00 REAL secs
Layer:   2 pwell    has      1444879 rects. |RTREE| = CPU        18.77 secs.        0.39 REAL secs
Layer:   3 scnmos   has      1110963 rects. |RTREE| = CPU        14.63 secs.        0.31 REAL secs
Layer:   4 scpmoshv has      1110963 rects. |RTREE| = CPU        13.99 secs.        0.29 REAL secs
Layer:   5 ndiff    has      7698513 rects. |RTREE| = CPU       107.26 secs.        2.39 REAL secs
Layer:   6 pdiff    has     12153635 rects. |RTREE| = CPU       166.60 secs.        3.86 REAL secs
Layer:   7 ndiffc   has      1951634 rects. |RTREE| = CPU        24.24 secs.        0.51 REAL secs
Layer:   8 pdiffc   has      3439988 rects. |RTREE| = CPU        48.29 secs.        1.05 REAL secs
Layer:   9 psubdiff has        90804 rects. |RTREE| = CPU         0.95 secs.        0.02 REAL secs
Layer:  10 nsubdiff has       136206 rects. |RTREE| = CPU         1.43 secs.        0.03 REAL secs
Layer:  11 psubdiff has        45402 rects. |RTREE| = CPU         0.42 secs.        0.01 REAL secs
Layer:  12 nsubdiff has        90804 rects. |RTREE| = CPU         0.92 secs.        0.02 REAL secs
Layer:  13 poly     has     11573460 rects. |RTREE| = CPU       158.96 secs.        3.48 REAL secs
Layer:  14 polycont has      1535008 rects. |RTREE| = CPU        22.76 secs.        0.49 REAL secs
Layer:  15 rmp      has            2 rects. |RTREE| = CPU         0.09 secs.        0.00 REAL secs
Layer:  16 ndiode   has         9459 rects. |RTREE| = CPU         0.11 secs.        0.00 REAL secs
Layer:  17 ndiodec  has         4204 rects. |RTREE| = CPU         0.04 secs.        0.00 REAL secs
Layer:  18 locali   has     22787594 rects. |RTREE| = CPU       323.56 secs.        7.28 REAL secs
Layer:  19 viali    has      3318330 rects. |RTREE| = CPU        49.53 secs.        1.17 REAL secs
Layer:  20 metal1   has      5365556 rects. |RTREE| = CPU       108.42 secs.        3.55 REAL secs
Layer:  21 via1     has      1026776 rects. |RTREE| = CPU        12.26 secs.        0.26 REAL secs
Layer:  22 metal2   has      1197256 rects. |RTREE| = CPU        34.53 secs.        0.95 REAL secs
Layer:  23 via2     has       170296 rects. |RTREE| = CPU         1.89 secs.        0.04 REAL secs
Layer:  24 metal3   has       139228 rects. |RTREE| = CPU         3.21 secs.        0.07 REAL secs
Layer:  25 via3     has       164730 rects. |RTREE| = CPU         1.71 secs.        0.04 REAL secs
Layer:  26 metal4   has        64915 rects. |RTREE| = CPU         0.97 secs.        0.02 REAL secs
Layer:  27 via4     has         2120 rects. |RTREE| = CPU         0.02 secs.        0.00 REAL secs
Layer:  28 metal5   has         1230 rects. |RTREE| = CPU         0.01 secs.        0.00 REAL secs
 +-----------------------------------------------------------------+
Layer:   1 nwell    has            0 non-rects          328 rects. |RTREE| = CPU         0.10 secs.        0.00 REAL secs
Layer:   2 pwell    has       590802 non-rects       105754 rects. |RTREE| = CPU        22.99 secs.        0.48 REAL secs
Layer:   3 scnmos   has            0 non-rects      1110963 rects. |RTREE| = CPU        16.36 secs.        0.34 REAL secs
Layer:   4 scpmoshv has            0 non-rects      1110963 rects. |RTREE| = CPU        15.03 secs.        0.32 REAL secs
Layer:   5 ndiff    has      1747614 non-rects        55556 rects. |RTREE| = CPU       115.16 secs.        2.78 REAL secs
Layer:   6 pdiff    has      1762312 non-rects        37956 rects. |RTREE| = CPU       175.43 secs.        4.61 REAL secs
Layer:   7 ndiffc   has            0 non-rects      1951634 rects. |RTREE| = CPU        25.08 secs.        0.54 REAL secs
Layer:   8 pdiffc   has            0 non-rects      3439988 rects. |RTREE| = CPU        52.06 secs.        1.19 REAL secs
Layer:   9 psubdiff has            0 non-rects        90804 rects. |RTREE| = CPU         1.04 secs.        0.02 REAL secs
Layer:  10 nsubdiff has            0 non-rects       136206 rects. |RTREE| = CPU         1.51 secs.        0.03 REAL secs
Layer:  11 psubdiff has            0 non-rects        45402 rects. |RTREE| = CPU         0.45 secs.        0.01 REAL secs
Layer:  12 nsubdiff has            0 non-rects        90804 rects. |RTREE| = CPU         0.91 secs.        0.02 REAL secs
Layer:  13 poly     has      1477285 non-rects      2215871 rects. |RTREE| = CPU       169.66 secs.        4.18 REAL secs
Layer:  14 polycont has            0 non-rects      1535008 rects. |RTREE| = CPU        20.26 secs.        0.43 REAL secs
Layer:  15 rmp      has            0 non-rects            2 rects. |RTREE| = CPU         0.35 secs.        0.01 REAL secs
Layer:  16 ndiode   has         1051 non-rects            0 rects. |RTREE| = CPU         0.11 secs.        0.00 REAL secs
Layer:  17 ndiodec  has            0 non-rects         4204 rects. |RTREE| = CPU         0.46 secs.        0.01 REAL secs
Layer:  18 locali   has       769008 non-rects       434820 rects. |RTREE| = CPU       346.15 secs.        8.68 REAL secs
Layer:  19 viali    has       201291 non-rects      2724384 rects. |RTREE| = CPU        50.49 secs.        1.12 REAL secs
Layer:  20 metal1   has       270926 non-rects       476738 rects. |RTREE| = CPU       117.07 secs.        4.01 REAL secs
Layer:  21 via1     has       157958 non-rects       268435 rects. |RTREE| = CPU        16.90 secs.        0.36 REAL secs
Layer:  22 metal2   has       235896 non-rects       350547 rects. |RTREE| = CPU        36.66 secs.        1.04 REAL secs
Layer:  23 via2     has        36123 non-rects        10040 rects. |RTREE| = CPU         2.84 secs.        0.06 REAL secs
Layer:  24 metal3   has        17517 non-rects         3246 rects. |RTREE| = CPU         4.01 secs.        0.08 REAL secs
Layer:  25 via3     has        32594 non-rects         2818 rects. |RTREE| = CPU         2.58 secs.        0.05 REAL secs
Layer:  26 metal4   has         1878 non-rects            0 rects. |RTREE| = CPU         1.42 secs.        0.03 REAL secs
Layer:  27 via4     has          319 non-rects          317 rects. |RTREE| = CPU         0.26 secs.        0.01 REAL secs
Layer:  28 metal5   has          198 non-rects            0 rects. |RTREE| = CPU         0.02 secs.        0.00 REAL secs
 +-------------------------- Design Extent ------------------------+
 Box: [           0 ,           0 ] to [      358194 ,      361448 ]
 +-----------------------------------------------------------------+
 Box: [        1066 ,        2437 ] to [      358194 ,      358779 ]
 Box: [        1105 ,        2159 ] to [      358155 ,      359057 ]
 Box: [        1183 ,        2223 ] to [      358077 ,      358993 ]
 Box: [        1183 ,        2473 ] to [      358077 ,      358743 ]
 Box: [        1131 ,        2223 ] to [      358129 ,      358993 ]
 Box: [        1131 ,        2473 ] to [      358129 ,      358743 ]
 Box: [        1139 ,        2235 ] to [      358121 ,      358977 ]
 Box: [        1139 ,        2491 ] to [      358121 ,      358684 ]
 Box: [        3709 ,        2240 ] to [      356655 ,      358976 ]
 Box: [        3709 ,        2481 ] to [      356655 ,      358735 ]
 Box: [        3709 ,        2287 ] to [      356655 ,      358929 ]
 Box: [        3709 ,        2505 ] to [      356655 ,      358711 ]
 Box: [        1155 ,        2197 ] to [      358105 ,      359019 ]
 Box: [        1171 ,        2223 ] to [      358089 ,      358839 ]
 Box: [      347321 ,        2408 ] to [      347555 ,        2417 ]
 Box: [        1503 ,        4391 ] to [      356013 ,      342897 ]
 Box: [        1512 ,        4409 ] to [      356005 ,      342879 ]
 Box: [        1104 ,        2159 ] to [      358156 ,      359057 ]
 Box: [        1133 ,        2159 ] to [      358127 ,      359057 ]
 Box: [         106 ,        2128 ] to [      358156 ,      359088 ]
 Box: [         112 ,        2150 ] to [      357860 ,      359066 ]
 Box: [         110 ,           0 ] to [      357860 ,      361448 ]
 Box: [         110 ,        2148 ] to [      356482 ,      359068 ]
 Box: [           0 ,        2143 ] to [      356487 ,      359073 ]
 Box: [        1532 ,        2144 ] to [      356348 ,      359072 ]
 Box: [        1531 ,        2128 ] to [      356349 ,      359088 ]
 Box: [        3838 ,        5388 ] to [      349258 ,      358138 ]
 Box: [        1104 ,        5346 ] to [      358156 ,      358180 ]
 +-----------------------------------------------------------------+
 
TODO:
0) Calculate AREA of boxes using vertex token scanning
0b)Assuming layer is overlap free, this should just be sum of box area
1) Fix INTERACT on point touch box and create test with HOLES
2) Single LAYER AND => during PNUM gives HEAL status
4) PNUM sort
4) AND optimization: compare bottom up RTREE to TOP Down
   Is it possible that RTree( x AND y ) = f( RTree(x), RTree(y) )
5) TOPOLOGICAL tables support
6) Read in the HDF5 by Julia
7) Control
program my_control
decl design input d1 = FPU.mag
decl design input d2 = STD.mag
decl design output o1 = FOO.mag
decl design output o2 = FOO.gds
decl int ctr = 2
decl real dmin = 3.0
decl layer diff = d1:65:20
decl layer poly = d1:66:20
var layer gate = ( diff * poly )
var layer source_drain = ( diff - poly )
decl group g1 = [ gate, source_drain ]
exec run g1
exec push g1 o1:gate
exec push g1 o2:10:0
end my_control

8) Support Vertex token and scanline

MW large design
Existing do concurrent: Sorting/OMT completed in     10248.56 CPU seconds.     2256.00 REAL seconds.
