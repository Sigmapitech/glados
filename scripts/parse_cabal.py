#!/usr/bin/env python3
import sys
import json
from pathlib import Path

def parse_cabal(path: Path):
    pkg_name = None
    exes = []
    try:
        with path.open(encoding="utf-8") as f:
            for raw in f:
                line = raw.rstrip("\n")
                stripped = line.lstrip()
                # ignore comments
                if stripped.startswith("--"):
                    continue
                low = stripped.lower()
                # name: field (top-level)
                if pkg_name is None and low.startswith("name:"):
                    # everything after first ':' is the name
                    pkg_name = stripped.split(":", 1)[1].strip()
                    continue
                # executable stanza header: starts with 'executable '
                if stripped.startswith("executable "):
                    # name is the rest after the keyword
                    exe_name = stripped[len("executable "):].strip()
                    if exe_name:
                        exes.append(exe_name)
    except Exception as e:
        print(f"error: cannot read {path}: {e}", file=sys.stderr)
    return pkg_name, exes

def main():
    repo = Path(".")
    cabal_files = list(repo.glob("*.cabal")) + list(repo.glob("*/*.cabal"))
    result = {}
    for cf in cabal_files:
        pkg, exes = parse_cabal(cf)
        if pkg:
            result[pkg] = {"file": str(cf), "executables": exes}
    # simple JSON output so Makefile or scripts can consume easily
    print(json.dumps(result))

if __name__ == "__main__":
    main()
