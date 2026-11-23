# cluster/ — heterogeneous compute-cluster prototype (Julia control plane)

A dummy application for prototyping distributed MAGPARSER DAG execution across
machines that **share no filesystem** and run **different architectures and MPI
implementations** (aarch64 GB10, aarch64 8 GB board, x86_64 RHEL8, macOS).

Design: Julia talks over plain TCP between nodes, and MPI runs only *inside* a node.
A single MPI world can't span Open MPI and Intel MPI binaries. A dead rank aborts the
whole world, and MPI doesn't handle nodes joining at runtime well. So the
cross-node layer is an Erlang/OTP-style supervisor written in plain Julia with
the standard library only (no HTTP.jl/JSON3/Serialization needed on remote nodes).

```
            ┌───────────────────────── coordinator.jl ─────────────────────────┐
            │ DAG scheduler · membership · journal · verify · dashboard :8080  │
            └──────▲───────────────────▲───────────────────▲───────────────────┘
        TCP :7700  │ (agents dial out) │                   │
            ┌──────┴──────┐     ┌──────┴──────┐     ┌──────┴──────┐
            │ agent.jl    │     │ agent.jl    │     │ agent.jl    │
            │ spark-local │     │ rhel8-A     │     │ JN6-TX2     │
            │ mpirun -np2 │     │ gfortran    │     │ ...         │
            │  dagop      │     │  dagop      │     │  dagop      │
            └─────────────┘     └─────────────┘     └─────────────┘
```

## Files
| file | role |
|---|---|
| `coordinator.jl` | membership, scheduler, retries, journal, ssh launcher, verification, cleanup |
| `agent.jl` | per-node daemon: builds the worker from source received over TCP, runs tasks |
| `Proto.jl` | little-endian framed wire protocol (arch/Julia-version independent) |
| `dagref.jl` | Julia reference implementation of the ops (verification, fallback, self-test) |
| `dashboard.jl`, `MiniJSON.jl` | stdlib HTTP server + JSON writer for the dashboard |
| `dummy/dagop.F90` | Fortran "MAGPARSER stand-in": `gen grow shrink and or area`, MPI inside a node |
| `dags/example.dag`, `dags/tiny.dag` | toy DAGs (`NAME = op inputs... k=v...`) |
| `hosts.toml.example` | real cluster config (spark-local, rhel8-A, JN6-TX2, Mac template) |
| `local_sim.toml`, `run_local_sim.sh` | 3 simulated nodes + 1 spare on one machine |

## How to run

### Prerequisites
- **Coordinator machine** (spark-bc08): Julia ≥ 1.10 using only its standard library, plus `tar`, `ssh` and `bash`.
  Port **7700/tcp** must be reachable from the other nodes.
- **Each node**: Julia ≥ 1.10 (1.11.3 and 1.13.4 are tested), `tar`, and optionally a
  Fortran compiler (`mpif90`/`mpifort`/`mpiifx`/`gfortran`). With no compiler the
  node uses the Julia reference worker. Nothing needs to be installed or copied by hand.
- **Passwordless ssh** from the coordinator to every node (`ssh -o BatchMode=yes HOST true`
  must succeed). Non-interactive ssh often has a minimal `PATH`, so give the full
  `julia` path in `hosts.toml` (e.g. JN6-TX2: `/home/skoranne/julia-1.11.3/bin/julia`).

### 1. Simulated cluster on one machine (no ssh)
```bash
cd cluster
./run_local_sim.sh --linger 60            # 3 agents (MPI / serial / Julia worker) + spare "sim-d"
# open http://127.0.0.1:8080/ ; try Add sim-d, Drain, Kill while the DAG runs
./run_local_sim.sh --cost-scale 0.3       # faster run
julia coordinator.jl --hosts local_sim.toml --dag dags/tiny.dag   # 7-task smoke test
```
The exit code is 0 only if every task finished and matched `dagref.jl`.

### 2. Real cluster (spark-bc08 + rhel8-A + JN6-TX2.local)
```bash
cd cluster
cp hosts.toml.example hosts.toml          # edit: [coordinator].advertise = this machine's LAN IP,
                                          # one [[host]] per node (ssh target, julia path, max_jobs, mpi_np)
ssh -o BatchMode=yes rhel8-A true && ssh -o BatchMode=yes JN6-TX2.local true    # sanity check
julia coordinator.jl --hosts hosts.toml --dag dags/example.dag --linger 120
```
The coordinator does the following:
1. ssh-copies `agent.jl Proto.jl dagref.jl` to `/tmp/vlsi_cluster_<run>_<host>_<n>` on every `autostart` host.
2. Starts the agents, which dial back to `advertise:7700`, register, receive `dagop.F90` and build it.
3. Runs the DAG, verifies every result and prints a per-task and per-node summary.
4. Shuts the agents down and runs `rm -rf` on every remote directory, including after Ctrl-C or an error.

Hosts with `autostart = false` appear on the dashboard as spares with an **Add** button.

### 3. Dashboard
It binds to `127.0.0.1:8080` only. From your laptop:
```bash
ssh -L 8080:127.0.0.1:8080 spark-bc08     # then browse http://127.0.0.1:8080/
```
`--http-bind 0.0.0.0` exposes it on the LAN, but there is no authentication.
The same actions are available as API calls: `curl -XPOST 'http://127.0.0.1:8080/api/drain?node=rhel8-A'`
(`drain`, `undrain`, `kill` with `node=`, `add` with `host=`), and `GET /api/state` returns JSON.

### 4. Agents started by hand (no ssh, e.g. the Mac)
```bash
# coordinator: fix the token so external agents can authenticate
julia coordinator.jl --hosts hosts.toml --dag dags/example.dag --token MYSECRET --linger 300
# any node: copy agent.jl Proto.jl dagref.jl over, then
VLSI_CLUSTER_TOKEN=MYSECRET julia agent.jl --coord 10.0.0.100:7700 --name macbook --max-jobs 2
```
The agent cleans up its temp workdir when it exits.

### 5. Coordinator restart (journal)
```bash
julia coordinator.jl --hosts hosts.toml --dag dags/example.dag --journal ~/cluster_journal --token T
# ... coordinator dies; agents keep retrying for 300 s ...
julia coordinator.jl --hosts hosts.toml --dag dags/example.dag --journal ~/cluster_journal --token T --no-launch
```
Finished tasks are restored from the journal and only the remaining ones run. Delete the journal dir when you are done with it.

### Coordinator options
| option | default | meaning |
|---|---|---|
| `--dag FILE` | (required) | DAG to run |
| `--hosts FILE` | none | hosts/coordinator config (without it, only hand-started agents join) |
| `--port`, `--bind` | 7700, 0.0.0.0 | agent listener |
| `--advertise ADDR` | from hosts.toml | address agents dial back to |
| `--http`, `--http-bind` | 8080, 127.0.0.1 | dashboard |
| `--token T` | random per run | shared secret agents must present |
| `--journal DIR` | none | persist results, so a restarted coordinator can resume |
| `--no-launch` | off | do not ssh-launch, only accept agents that dial in |
| `--linger S` | 0 | keep the dashboard up S seconds after the DAG finishes |
| `--cost-scale X` | 1.0 | multiply every `cost_ms` in the DAG |
| `--local-fallback-after S` | 30 | with no live nodes for S seconds, run tasks on the coordinator |
| `--keep-remote` | off | debugging: leave remote dirs (agent.log) in place |

### Agent options (normally set from `hosts.toml`)
`--coord H:P[,H2:P2]` `--name` `--workdir` `--max-jobs` `--mpi-np` `--mem-cap-mb`
`--reserve-mb` `--fortran auto|<fc>|none` `--orphan-timeout 300` `--task-timeout 600`
`--chaos P` (crash with probability P per task) `--selfdestruct DIR`.

### Writing a DAG
```
# NAME = op [inputs defined above...] [param=value...]   (cost_ms = simulated cost)
A  = gen seed=1 n=2000 extent=50000 maxsize=300 cost_ms=500
GA = grow A d=10
C  = and GA x1=1000 y1=1000 x2=40000 y2=40000
AR = area C
```
Ops: `gen seed n extent maxsize`, `grow d`, `shrink d`, `and x1 y1 x2 y2`, `or` (2 inputs), `area`.

### Troubleshooting
- **A node never joins:** check that the node can reach port 7700 on the coordinator
  (`bash -c 'echo > /dev/tcp/10.0.0.100/7700'` on the node). Then rerun with `--keep-remote` and read
  `/tmp/vlsi_cluster_*/agent.log` on that node.
- **A node shows `worker=julia` when you expected Fortran:** the dashboard and the `ready` event show the
  compiler and self-test notes. Fix the compiler, or pin it with `fortran = "gfortran"` in `hosts.toml`.
- **Leftover dirs after a hard crash:** they are swept after 12 h on the next launch. To remove them now,
  run `rm -rf /tmp/vlsi_cluster_*` on the node.

## What it demonstrates
- **No NFS**: the coordinator pipes `agent.jl Proto.jl dagref.jl` through ssh into
  `$tmp/vlsi_cluster_<run>_<host>_<n>`. The worker *source* is sent over TCP and
  compiled on each node. The agent probes `mpif90 → mpifort → mpiifx → gfortran …`,
  checks the build against `dagref.jl`, and falls back from MPI to serial to the Julia worker.
  Task inputs and outputs travel inside the protocol frames.
- **Heterogeneous**: x86_64 and aarch64 nodes, Open MPI on one and serial gfortran on another,
  all produce byte-identical results, and the coordinator checks every result against the reference.
- **Failure recovery**: a node goes from suspect (6 s without a heartbeat) to dead (10 s, or socket closed).
  Its in-flight tasks go back in the queue with backoff. Each task gets 3 attempts, then the coordinator runs it locally.
  Each registration gets a new incarnation number, and late results from an earlier incarnation are ignored.
  Agents reconnect with backoff, trying every listed coordinator, and exit after 300 s with no coordinator.
- **Elastic membership**: the dashboard has **Add** (ssh-launch a host from
  `hosts.toml`, including `autostart=false` spares), **Drain** (finish current work, then exit),
  **Kill** (chaos crash), and hand-started agents join at any time.
- **Memory guard** (8 GB boards): the coordinator only sends a task to a node whose
  reported available memory fits it. The agent also checks `4×input + 64 MB` against
  `MemAvailable − reserve` and rejects the task if it doesn't fit, and the scheduler then places it elsewhere.
- **Coordinator restart**: with `--journal DIR`, finished results are stored by content hash.
  Restart with the same journal, `--token` and `--no-launch`. Finished tasks are restored and the orphaned agents reconnect.
- **Cleanup**: every task runs in a temp dir that is always removed. On SHUTDOWN an agent removes its own dir.
  At exit the coordinator kills each agent it launched and runs `rm -rf` on its dir. Leftover `vlsi_cluster_*`
  dirs older than 12 h are swept on launch.

## Tested (2026-11-22/23)
| scenario | result |
|---|---|
| local sim, `example.dag` (32 tasks, 3 worker modes) | PASSED, 34 s, no leftovers |
| kill -9 agent, Add spare, Drain, dashboard Kill, re-Add killed host | all requeued, PASSED, no leftovers |
| coordinator kill -9 at 22 s, restart from journal | 20 tasks restored, agents reconnected, PASSED |
| agent reserve > RAM | rejects → rescheduled, PASSED |
| spark-local (aarch64, Open MPI np=2) + rhel8-A (x86_64, gfortran) | 14 + 18 tasks, PASSED, remote `/tmp` cleaned |
| + JN6-TX2.local (aarch64 8 GB, Open MPI np=2, Julia 1.11.3 vs 1.13.4 coordinator) | 17 + 12 + 3 tasks, PASSED, all remote `/tmp` cleaned |

## Next steps
- JN6-TX2 needs about 10 s after joining to build and self-test the MPI worker, so it misses the early tasks.
  Builds could be cached across runs, keyed by the source SHA.
- A `magparser` op: ship a `.ctr` file and its `.bin` inputs, run the host's `MAGPARSER.exe`, and return the output `.bin` files.
- Mark DAG nodes that can be offloaded from the CTR/TRS pass (`src/trs_optimizer.f90`).
- Hot-standby coordinator (agents already accept a list in `--coord a:7700,b:7700`).
- Authentication is a shared token only, and frames are not encrypted. Keep it on the LAN, or tunnel it through ssh.
