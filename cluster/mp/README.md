# cluster/mp — MAGPARSER RPC cluster on Julia Distributed.jl

This runs MAGPARSER DAG operations (OR, AND, NOT, XOR, GROW, SHRINK, INTERACT, ...) on
remote machines. It does not use CTR files, BIN files, scp or NFS:

```
 coordinator (mpcoord.jl, Julia master)
   │  Distributed.jl remotecall over ssh tunnel:  op, params, CompactLayer "CLW1" bytes
   ▼
 worker (MPWorker.jl, shipped through the Distributed connection)        one per slot
   │  loopback TCP, binary request/response with CLW1 blobs
   ▼
 MAGPARSER.exe --rpc-serve 127.0.0.1:PORT   (long-lived; runs the normal Calculate* code)
```

- **Wire format**: `CLW1` is the CompactLayer network form (`src/compact_wire.f90`, Julia
  codec `CompactWire.jl`). It is a 96-byte little-endian header followed by the shape table,
  then either the cold bit-packed blocks (form 1) or the hot sorted keys (form 0). Wide
  layers (separate shape-id stream) are supported. Raw boxes (form 2) are used only when
  `CompactEncode` cannot encode a layer. The Julia encoder matches `CompactEncode`+`CompactPack`
  bit for bit: Fortran decodes a Julia-made layer and re-encodes it to identical bytes.
- **MAGPARSER side** (`src/`): `rpc_socket.c` (POSIX socket shim), `compact_wire.f90`,
  `rpc_server.f90`, and a `--rpc-serve HOST:PORT` branch in `mainmagparser.f90`. The server
  expands each input with `LayerExpand`, calls the same routines as the CTR interpreter
  (`CalculateBoostOperation`, `CalculateAND`, `CalculateNOT`, `CalculateGROWLayer`,
  `CalculateSHRINKLayer`, `CalculateTopologicalSelect`, ...) and returns the result as cold
  CompactLayer. It writes no files. A crash or `error stop` only kills that server; the worker
  reports the error and starts a fresh server for the next request.
- **Why not MPI between machines**: spark (Open MPI) and rhel8-A (Intel MPI) cannot share
  an MPI world, and MPI does not survive node loss. MPI and OpenMP stay inside each MAGPARSER.

## hosts file
Every host must give **full paths** to `julia` and `MAGPARSER.exe` (see `mp_hosts.toml.example`):
```toml
[[host]]
name = "rhel8-A"
ssh = "rhel8-A"                     # or "local"
julia = "/home/skoranne/.juliaup/bin/julia"
magparser = "/home/skoranne/GITHUB/VLSI_LAYOUT/src/MAGPARSER.exe"
workers = 2                         # concurrent ops = MAGPARSER servers on this host
tmp = "/tmp"                        # per-run scratch dirs vlsi_mp_<run>_* are deleted
env = { OMP_NUM_THREADS = "16" }    # passed to MAGPARSER
# reserve_mb = 1024, autostart = false (spare: "Add" on the dashboard)
```
The coordinator checks every host before using it. A host is **removed for this run**,
with a `NOTE:` line and a dashboard entry explaining why, when:
- the hosts file has no `julia`/`magparser` path, or the host is unreachable;
- `MAGPARSER.exe` is missing or not executable;
- the node's Julia major.minor differs from the coordinator's (Distributed.jl requirement);
- MAGPARSER exits or does not connect back (a binary built before `--rpc-serve` existed).

## Run
```bash
cd cluster/mp
julia mpcoord.jl --hosts mp_local_sim.toml --dag dags/mp_example.dag --linger 60   # 2 local nodes
cp mp_hosts.toml.example mp_hosts.toml                                              # real cluster
julia mpcoord.jl --hosts mp_hosts.toml --dag dags/mp_example.dag
```
Options: `--http 8080`, `--http-bind 127.0.0.1`, `--journal DIR` (restartable),
`--verify cross|none` (default `cross`), `--linger S`.
The dashboard (http://127.0.0.1:8080, use `ssh -L` from another machine) is shared with
`cluster/`. It has **Drain**, **Kill** (crashes the node's workers) and **Add** (start a spare host).

DAG syntax (`dags/mp_example.dag`, `dags/mp_chaos.dag`):
```
M1  = gen seed=11 n=200000 extent=2000000 maxsize=800    # generated on the coordinator
G1  = GROW M1 d=20                                       # or e= n= w= s=
GV  = AND G1 VIA
IV  = INTERACT M1 VIA
CHK = XOR D A12                                          # expect 0 boxes
```

## CLUSTER mode: running a MAGPARSER `.ctr` file
`--ctr FILE` takes an ordinary MAGPARSER control file in place of `--dag` (`CtrFront.jl`). Each
`exec run` line becomes one cluster task, and the result layers are written where MAGPARSER
would write them, so the same `.ctr` gives the same output files in both modes:
```bash
cd /some/run/dir                       # .ctr paths are relative to the current directory, as in MAGPARSER
julia ~/GITHUB/VLSI_LAYOUT/cluster/mp/tools/genmag.jl cluster_bool_in.mag 200000
cp ~/GITHUB/VLSI_LAYOUT/cluster/mp/ctr/{cluster_boolean.ctr,cluster_bool_out.mag} .
julia ~/GITHUB/VLSI_LAYOUT/cluster/mp/mpcoord.jl --hosts mp_hosts.toml --ctr cluster_boolean.ctr \
      --log run.log --tui-file dash.txt
```
| CTR | cluster op | note |
|---|---|---|
| `A * B` | `AND` | intersection |
| `A + B` | `OR` | Boost OR |
| `A ^ B` | `XOR` | Boost XOR |
| `A @ B` | `NOT` | frame NOT |
| `A ! B` | `BOOSTNOT` | Boost NOT |
| `A ~ B`, `A FRAMENOT B` | `FRAMENOT` | |
| `A NOT B`, `A XOR B`, `A OR2 B`, `A AND2 B` | same name | `AND2` = frame AND |
| `A INTERACT B`, `A NOT INTERACT B`, `A COINCIDENT INSIDE EDGE B`, ... | `INTERACT`, `NOT_INTERACT`, ... | one- or multi-word topological selects |
| `A AND B`, `A AND nothing` | `MERGE` | CTR word `AND` is the single-layer merge of A, not intersection |
| `A EXTENT nothing`, `A COPY nothing`, `A GRID nothing` | same name | |
| `A GROW nothing e n w s` | `GROW` | microns ×1000 → DBU, truncated as in MAGPARSER |
| `A SHRINK nothing d` or `e n w s` | `SHRINK` | |
| `A SIZE nothing d`, `A SIZE B d` | `SIZE`, `SIZE2` | |
| `A ASSERT_ZERO nothing` | checked on the coordinator | logs `ASSERT_ZERO fail`; fails the run when `var abort_on_assert_zero` > 1 |

- `decl design dN = f.mag` reads a flat Magic file, either `rect` lines or MAGPARSER's
  `KLBIN f.bin` layer sections. Every `dN:layer` that is used becomes a source layer on the coordinator.
- `decl output oN = f.mag` works as in MAGPARSER. If `f.mag` is a template that maps layers to
  `KLBIN f.bin`, each `oN:layer` result is written to its `.bin` (raw int32 `x1 y1 x2 y2`).
  Otherwise `f.mag` is written as a flat `rect` layout.
- Earlier results (`m1:x`, `o1:x`) can be inputs, so independent lines run in parallel on
  different nodes, and chains wait for their inputs.
- `var cluster_tile_boxes N` and `var cluster_tiles N` control tiles (below);
  `var abort_on_assert_zero N` is honoured. MAGPARSER itself prints an INFO line for the two
  cluster variables and otherwise ignores them, so the `.ctr` still runs unchanged there.
- `program`, `systeminfo`, `info`, other `var`s, `exec init`, `flush` and `end` are accepted. Any other
  line (parenthesised expressions, loops, `decl` kinds other than design/output/memory) stops
  the run with its line number, so a CTR never runs half-understood.
- After bring-up the log says where the job runs:
  `CLUSTER MODE: running distributed on 3 nodes: JN6-TX2 (aarch64, 1 worker), rhel8-A (x86_64, 2 workers), ...`
- `tools/compare_klbin.jl CLUSTER_DIR TRUTH_DIR` checks the `.bin` outputs against a plain
  `MAGPARSER.exe file.ctr 20` run as sorted box sets. CLW1 is Morton-ordered, so the file bytes
  differ while the boxes are the same.

### Tiles: splitting one large op across nodes
One very large AND/OR/NOT/XOR does not have to run on one node. When the input layers of an
`OR AND NOT XOR BOOSTNOT FRAMENOT GROW SIZE` task hold more than **`cluster_tile_boxes`** boxes
(CTR `var cluster_tile_boxes N`, default **10 000 000**; `--tile-boxes N` overrides, 0 disables),
the coordinator splits the task into y-band tiles, using the same tile manager as MAGPARSER's own
banded execution (`src/band_exec.f90`):

| step | MAGPARSER, one machine (`BandedRun`) | CLUSTER mode |
|---|---|---|
| `TilePlan` | band edges at equal-count quantiles of the inputs' y centres | the same, in `mpcoord.jl` `tile_plan` |
| `TileExtract` | whole boxes meeting `[lo - halo, hi + halo]` | the same, `tile_extract`; only those boxes are sent |
| `TileApply` + `TileClip` | op on the band, result clipped to `[lo, hi]` | RPC op `TILE_<op>` on any node (`rpc_server.f90`) |
| `TileCommit` | compact parts merged into the output layer | clipped tile results concatenated on the coordinator (bands are disjoint) |

- Tile count: `max(ceil(boxes / cluster_tile_boxes), worker slots)`, or exactly
  `var cluster_tiles N` / `--tiles N`. Tiles that are empty for the op (AND with an empty side,
  NOT with an empty A) are not sent.
- Halo: 0 for the Boolean ops, `max(e, n, w, s)` for GROW, `|d|` for SIZE, so every result is exact.
- The tiles are ordinary tasks named `out#k`: they are scheduled on any free worker, retried,
  journalled and cross-verified on another node. The tiled task is verified through its tiles
  (`✓ 4/4 tiles verified`).
- Results equal the untiled op as a region. Rectangles are split at band edges, so box counts
  differ slightly (`tools/compare_klbin.jl` then checks region equality with MAGPARSER XOR).

Log lines (`--log`):
```
tile   o1:t_and AND: 400000 input boxes > cluster_tile_boxes 100000 -> 4 tiles (4 non-empty), <= 100080 boxes/tile, halo 0 DBU; split 0.19s, 5.14 MiB of tile inputs
done   o1:t_and#1 TILE_AND on sim-a/pid 4: 1694 boxes (13.84 B/box) | REMOTE RUNTIME MAGPARSER 0.051s, RPC 0.054s | NETWORK sent 1.29 MiB, received 22.9 KiB
join   o1:t_and AND: 4 tiles on sim-a,sim-b -> 6605 boxes in 3.76s | REMOTE RUNTIME MAGPARSER sum 0.153s (max tile 0.051s), RPC sum 0.164s | NETWORK sent 5.14 MiB, received 89.5 KiB
acct   TOTAL work     48 calls | REMOTE RUNTIME MAGPARSER 4.701s, RPC 5.888s | NETWORK sent 69.44 MiB, received 36.36 MiB (105.79 MiB)
```
### REMOTE RUNTIME and NETWORK accounting
Every remote call logs **REMOTE RUNTIME**: the MAGPARSER time measured on the node, and the RPC
round trip measured by the coordinator (MAGPARSER time + transfer + (de)serialisation). It also
logs **NETWORK**: the CLW1 bytes sent to the node and received from it. At the end, `acct` lines
give per-node and run totals, split into `work` (the DAG) and `verify` (the cross-checks).
On the coordinator's own host the bytes go over loopback.

`ctr/cluster_tiles.ctr` (with `ctr/cluster_tiles_out.mag`) exercises tiles and the remaining ops.

## ASCII dashboard
`../tui.jl` renders the same state as the HTML dashboard as text: a progress bar, the
`running distributed on N nodes` banner, per-node load and memory bars with the tasks they are
running, the task table and the last events.
- `--tui auto|on|off` (default `auto`): on a terminal the dashboard redraws in place every second,
  and the event log goes only to `--log FILE`.
- `--tui-file F` appends a timestamped frame every second (for non-terminal runs and for logs).

## Failure handling
| event | behaviour |
|---|---|
| MAGPARSER server crashes mid-op | the worker throws with MAGPARSER's log tail; the task is retried (up to 3 attempts) and a fresh server is started |
| worker or whole node dies | Distributed reports `ProcessExitedException`; the in-flight task goes back in the queue on another worker |
| node stops answering heartbeats | suspect after 8 s, removed after 20 s (`rmprocs`), its tasks go back in the queue |
| Add / Drain at runtime | `addprocs` + code shipping / no new work, then `rmprocs` once idle |
| coordinator restart | `--journal DIR` restores finished results by content hash |

**Verification**: each result is recomputed on another node (a different architecture when
available) and `XOR(result, recomputed)` must be empty. The summary also says whether the
two results are byte-identical. This is region equality, so builds that split the region
into boxes differently (nvfortran vs ifx) still verify.

## Tested (2026-11-23, spark-bc08)
| test | result |
|---|---|
| Julia ⇄ Fortran CLW1 | Fortran decodes a Julia-made layer and re-encodes it to identical bytes (`test_compact_wire.exe`) |
| all ops over RPC | OR/AND/NOT/XOR/GROW/SHRINK/INTERACT/EXTENT ok, COPY returns the input set, unknown op gives status 1 |
| `mp_example.dag`, 2 local nodes | 13 ops in 2.8 s, all cross-verified byte-identical, `CHK` = 0 boxes |
| `mp_chaos.dag` (1.5M-box layers): kill -9 a MAGPARSER, Kill node, Add spare, Drain | retried, requeued, finished, all 24 results verified, no leftovers |
| real hosts file, before the rhel8-A rebuild | JN6-TX2 removed (no MAGPARSER.exe), rhel8-A removed (MAGPARSER predates `--rpc-serve`), DAG ran on spark-local, remote `/tmp` clean |
| spark-local (aarch64, nvfortran) + rhel8-A (x86_64, ifx 2025.0, dc5172a) | 13 ops: 9 on spark, 4 on rhel8-A; every result recomputed on the other architecture is **byte-identical**; `CHK` = 0 boxes; remote `/tmp` clean |
| CLUSTER mode, `ctr/cluster_boolean.ctr`, 3 simulated local nodes (5k rects/layer) | 16 Boolean ops, all cross-verified; output `.bin` = plain MAGPARSER box for box |
| CLUSTER mode, **3 computers**: spark-local (nvfortran 26.1 GPU), rhel8-A (ifx 2025.0), JN6-TX2 (gfortran 13.4), 200k+200k+100k rects | 16 ops: 11 on spark, 4 on rhel8-A, 1 on JN6-TX2 (9.1 s), all cross-verified byte-identical on another node; vs plain MAGPARSER: 13 identical box sets + 3 NOT layers equal as regions (`tools/compare_klbin.jl` with MAGPARSER XOR, negative control 202053 boxes); remote `/tmp` clean |
| local banded refactor (`TilePlan/TileExtract/TileApply/TileClip/TileCommit`), OASIS input, `MAGPARSER_COMPACT_LAYERS=1`, 50k boxes/band | 7 banded ops (8 bands); all 17 layers **box-identical** to the pre-refactor `BandedRun`, equal as regions to the unbanded run |
| tiles + remaining ops, `ctr/cluster_tiles.ctr`, **3 computers** (c1c2f60 + tiles on all), `cluster_tile_boxes 100000` | 11 ops split into 5 tiles each (AND OR XOR NOT BOOSTNOT FRAMENOT, word NOT/XOR, GROW halo 50, SIZE halo 30), plus OR2 AND2 SHRINK INTERACT NOT_INTERACT INSIDE COPY ASSERT_ZERO; 62 remote results cross-verified; vs plain MAGPARSER: 17/17 layers equal (5 identical box sets, 12 equal regions); work 62 calls, MAGPARSER 7.8 s, 106.5 MiB on the network |
| one large AND/OR/XOR/NOT, `ctr/cluster_tiles_big.ctr`, **3 computers**, 2M + 2M rects, `cluster_tile_boxes 1000000` | each op split into 5 tiles of ≤ 800 779 boxes, tiles ran on JN6-TX2, rhel8-A and spark-local; DAG 14.3 s; all 20 tiles cross-verified; vs plain MAGPARSER: 4/4 equal regions (negative control 2 002 780 boxes); work 20 calls, MAGPARSER 19.1 s, 352 MiB on the network; remote `/tmp` clean |

A node's heartbeat clock now starts when monitoring starts. Before that, a node that joined
early (spark) could be declared dead at once if a slow node (JN6-TX2, about 20 s to join) held
up the start.

Remote ops are limited by the network, not MAGPARSER. On rhel8-A, OR on 2×200k boxes took
0.69 s in MAGPARSER but 7.9 s in total, for about 10 MB of CLW1 in and out. Raw ssh to
rhel8-A runs at about 7–9 MB/s, and the Distributed tunnel adds overhead on top. Send ops
whose compute is large relative to their CLW1 size.

## Notes
- **JN6-TX2 (Jetson TX2, Ubuntu 18.04, ARMv8.0)** runs MAGPARSER built with **gfortran 13.4**
  (`src/Makefile.gfortran`, CPU only). The GPU hosts keep nvfortran (`Makefile.nvf`). No NVHPC
  release works on the TX2:
  - NVHPC ≥ 23.1 runtimes (libnvomp, libnvmalloc, libnvcpumath) use ARMv8.1 LSE atomics
    (`ldaddl`), which the Cortex-A57/Denver2 cores lack, so every binary dies with SIGILL at startup.
  - NVHPC 22.1 (the oldest arm64 NGC image) has an LSE-free libnvomp and runs there, but its
    front end lacks the F2008 `shiftl`/`shiftr` intrinsics that MAGPARSER uses.
  - The system gfortran 7.5 lacks OpenMP 5 (`use_device_addr`) and the `omp_target_*` Fortran interfaces.

  Toolchain under `~` with no sudo: micromamba plus conda-forge (glibc 2.17 sysroot):
  ```bash
  ~/micromamba/bin/micromamba create -n gf13 -c conda-forge gfortran=13 gxx=13 snappy zlib zstd libboost-headers
  E=~/micromamba/envs/gf13; export PATH=$E/bin:$PATH CPATH=$E/include LIBRARY_PATH=$E/lib
  make -f Makefile.gfortran CC=gcc BOOST_DIR=$E/include LDFLAGS="-Wl,-rpath,$E/lib" MAGPARSER.exe
  ```
  Build serially, or rerun `make -j`, if it reports missing `.mod` files: `DEPENDENCIES.txt` does
  not list every module edge.
- gfortran portability edits are guarded or behave the same everywhere:
  - `boost_polygon_apif.f90` passes `c_loc` under `__GFORTRAN__`, because Box has type-bound
    procedures and so is not C interoperable;
  - `device_registry.f90` declares `bind(C)` libgomp interfaces for gfortran < 10;
  - `design.f90` and `design_device_impl.f90` put `c_loc` results in temporaries;
  - `band_exec.f90` takes a pointer instead of a component of a function result.
- `make -f Makefile.nvf GPU=0` is an nvfortran build with no CUDA: no `-cuda`/`-mp=gpu`, the
  nvfortran macros undefined, and the `.cu` files compiled as C++ stubs.
- nvfortran 26.1 mis-evaluates `ichar(c, int8)` inside a parameter array constructor, so
  `compact_wire.f90` and `rpc_server.f90` write the magic bytes as integer literals.
- The `CLW1` format follows the `CompactLayer` type, including the `wide`/`sid`/`sidpack` fields.
  If that type changes, update `compact_wire.f90` and `CompactWire.jl` together.
- `make -f Makefile.nvf test_compact_wire.exe` builds the CLW1 round-trip test.
