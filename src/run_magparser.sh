#!/usr/bin/env bash
# Run MAGPARSER.exe with platform-appropriate environment settings.
# Supports Linux (x86_64/aarch64/ARM), and macOS (Intel/Apple Silicon).
#
# Usage:
#   ./run_magparser.sh <CONTROL_FILE.ctr> [MAX_LAYERS]
#   ./run_magparser.sh <file.mag> [MAX_LAYERS]
#
# Environment overrides:
#   MAGPARSER_EXE   - path to MAGPARSER.exe (default: auto-detect)
#   INSTALL_PATH    - repo/install root containing bin/MAGPARSER.exe (fallback)
#   DYLD_LIBRARY_PATH / LD_LIBRARY_PATH can be pre-set to add custom libs.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

OS="$(uname -s)"
ARCH="$(uname -m)"

# OpenMP runtime defaults (can be overridden by caller)
export OMP_PROC_BIND="${OMP_PROC_BIND:-TRUE}"
export OMP_PLACES="${OMP_PLACES:-cores}"
# Enable parallel execution by default when OpenMP is available.
if [[ -z "${OMP_NUM_THREADS:-}" ]]; then
  if command -v sysctl >/dev/null 2>&1; then
    OMP_NUM_THREADS="$(sysctl -n hw.ncpu 2>/dev/null || echo 1)"
  elif command -v nproc >/dev/null 2>&1; then
    OMP_NUM_THREADS="$(nproc 2>/dev/null || echo 1)"
  elif command -v getconf >/dev/null 2>&1; then
    OMP_NUM_THREADS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"
  else
    OMP_NUM_THREADS=1
  fi
  export OMP_NUM_THREADS
fi

# KLayout (used by some regressions/conversion workflows)
if [[ -z "${KLAYOUT:-}" ]]; then
  if command -v klayout >/dev/null 2>&1; then
    export KLAYOUT="$(command -v klayout)"
  elif [[ "$(uname -s)" == "Darwin" && -x "/Applications/KLayout/klayout.app/Contents/MacOS/klayout" ]]; then
    export KLAYOUT="/Applications/KLayout/klayout.app/Contents/MacOS/klayout"
  fi
fi

# Locate executable: explicit > local src build > repo bin > PATH
find_exe() {
  if [[ -n "${MAGPARSER_EXE:-}" ]]; then
    echo "$MAGPARSER_EXE"; return 0
  fi
  if [[ -x "$SCRIPT_DIR/MAGPARSER.exe" ]]; then
    echo "$SCRIPT_DIR/MAGPARSER.exe"; return 0
  fi
  if [[ -n "${INSTALL_PATH:-}" && -x "$INSTALL_PATH/bin/MAGPARSER.exe" ]]; then
    echo "$INSTALL_PATH/bin/MAGPARSER.exe"; return 0
  fi
  if [[ -x "$REPO_ROOT/bin/MAGPARSER.exe" ]]; then
    echo "$REPO_ROOT/bin/MAGPARSER.exe"; return 0
  fi
  if command -v MAGPARSER.exe >/dev/null 2>&1; then
    command -v MAGPARSER.exe; return 0
  fi
  return 1
}

EXE="$(find_exe || true)"
if [[ -z "$EXE" ]]; then
  echo "ERROR: MAGPARSER.exe not found. Set MAGPARSER_EXE or build it (e.g. in src/)." >&2
  exit 1
fi

# Add a directory to a path-like variable if not already present
_add_path() {
  local var="$1" dir="$2"
  [[ -d "$dir" ]] || return 0
  local cur="${!var:-}"
  case ":$cur:" in
    *":$dir:"*) ;;
    *) export "$var"="${dir}${cur:+:$cur}" ;;
  esac
}

case "$OS" in
  Darwin)
    # macOS (Intel x86_64 or Apple Silicon arm64)
    # Common locations for ifort runtime and libs used by src/Makefile.mac
    _add_path DYLD_LIBRARY_PATH "/opt/intel/oneapi/compiler/2022.2.1/mac/compiler/lib"
    _add_path DYLD_LIBRARY_PATH "/opt/intel/oneapi/compiler/latest/mac/compiler/lib"
    _add_path DYLD_LIBRARY_PATH "/opt/homebrew/lib"
    _add_path DYLD_LIBRARY_PATH "/usr/local/lib"
    _add_path DYLD_LIBRARY_PATH "/usr/local/opt/zstd/lib"
    _add_path DYLD_LIBRARY_PATH "/usr/local/opt/snappy/lib"
    # Snappy from Intel Python env (as used in this workspace build), if present
    _add_path DYLD_LIBRARY_PATH "/opt/intel/oneapi/intelpython/python3.9/envs/2022.2.1/lib"

    echo "Platform: macOS ($ARCH)"
    ;;
  Linux)
    # Linux (x86_64 or ARM/aarch64)
    # NVIDIA HPC SDK (nvfortran builds, common on ARM servers like GB10)
    NVF_BIN="${NVF_BIN:-/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/bin}"
    [[ -d "$NVF_BIN" ]] && export PATH="$NVF_BIN:$PATH"
    _add_path LD_LIBRARY_PATH "/opt/nvidia/hpc_sdk/Linux_aarch64/26.1/compilers/lib"
    _add_path LD_LIBRARY_PATH "/opt/nvidia/hpc_sdk/Linux_x86_64/26.1/compilers/lib"
    _add_path LD_LIBRARY_PATH "/opt/nvidia/hpc_sdk/Linux_aarch64/latest/compilers/lib"
    _add_path LD_LIBRARY_PATH "/opt/nvidia/hpc_sdk/Linux_x86_64/latest/compilers/lib"
    # Intel oneAPI runtimes (ifx/ifort builds)
    _add_path LD_LIBRARY_PATH "/opt/intel/oneapi/compiler/latest/lib"
    _add_path LD_LIBRARY_PATH "/opt/intel/oneapi/compiler/latest/linux/compiler/lib/intel64_lin"
    _add_path LD_LIBRARY_PATH "/usr/local/lib"
    _add_path LD_LIBRARY_PATH "/usr/lib64"

    echo "Platform: Linux ($ARCH)"
    ;;
  *)
    echo "Platform: $OS ($ARCH) (unrecognized; attempting to run without extra library paths)"
    ;;
esac

echo "Running: $EXE $*"
exec "$EXE" "$@"
