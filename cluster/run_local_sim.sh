#!/usr/bin/env bash
# Run the prototype as a simulated 3-node cluster on this machine.
#   ./run_local_sim.sh [extra coordinator options, e.g. --linger 120 --cost-scale 0.5]
# Dashboard: http://127.0.0.1:8080/
cd "$(dirname "$0")"
exec julia --startup-file=no coordinator.jl --hosts local_sim.toml --dag dags/example.dag "$@"
