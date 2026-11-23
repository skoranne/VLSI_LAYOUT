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
| real hosts file | JN6-TX2 removed (no MAGPARSER.exe), rhel8-A removed (MAGPARSER predates `--rpc-serve`), DAG ran on spark-local, remote `/tmp` clean |

## Notes
- nvfortran 26.1 mis-evaluates `ichar(c, int8)` inside a parameter array constructor, so
  `compact_wire.f90` and `rpc_server.f90` write the magic bytes as integer literals.
- The `CLW1` format follows the `CompactLayer` type, including the `wide`/`sid`/`sidpack` fields.
  If that type changes, update `compact_wire.f90` and `CompactWire.jl` together.
- `make -f Makefile.nvf test_compact_wire.exe` builds the CLW1 round-trip test.
