import os
import sys
import glob
import subprocess

def find_cabal_file(start_path):
    """Walks up the directory tree to find the nearest .cabal file."""
    current = os.path.abspath(start_path)
    while True:
        if not os.path.exists(current):
            return None

        cabal_files = glob.glob(os.path.join(current, "*.cabal"))
        if cabal_files:
            return cabal_files[0]

        parent = os.path.dirname(current)
        if parent == current:
            return None
        current = parent

def get_test_suites(cabal_path):
    """Extracts test-suite names from a .cabal file."""
    suites = []
    try:
        with open(cabal_path, 'r') as f:
            for line in f:
                line = line.strip()
                if line.lower().startswith('test-suite'):
                    # format: test-suite <name>
                    parts = line.split()
                    if len(parts) >= 2:
                        suites.append(parts[1])
    except Exception as e:
        print(f"Error reading {cabal_path}: {e}")
    return suites

def run_coverage(suite_name):
    """Runs the test and coverage generation commands via Nix."""
    print(f"--> Running tests for suite: {suite_name}")

    test_cmd = f"nix develop --command cabal test {suite_name} --enable-coverage"
    res = subprocess.run(test_cmd, shell=True)
    if res.returncode != 0:
        print(f"!! Tests failed for {suite_name}")
        sys.exit(1)

    print(f"--> Generating Codecov report for: {suite_name}")
    report_file = f"coverage/codecov-{suite_name}.json"
    gen_cmd = f"nix develop --command hpc-codecov cabal:{suite_name} -o {report_file}"
    subprocess.run(gen_cmd, shell=True)

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 coverage_manager.py <changed_files_list>")
        sys.exit(1)

    changed_files = sys.argv[1].split()

    config_triggers = ["scripts/coverage_manager.py", ".github/workflows/codecov.yml"]
    full_scan = any(
        any(trigger in f for trigger in config_triggers)
        for f in changed_files
    )

    target_cabal_files = set()

    if full_scan:
        print("Configuration changed. Scanning ALL packages...")
        # Find all .cabal files, ignoring dist-newstyle
        for root, _, files in os.walk("."):
            if "dist-newstyle" in root:
                continue
            for file in files:
                if file.startswith('lisp') or file.startswith('poc'):
                    continue
                if file.endswith(".cabal"):
                    target_cabal_files.add(os.path.join(root, file))
    else:
        print("Scanning impacted packages only...")
        for f in changed_files:
            if not os.path.exists(f):
                continue

            cabal_file = find_cabal_file(os.path.dirname(f))
            if cabal_file:
                target_cabal_files.add(cabal_file)

    if not target_cabal_files:
        print("No relevant Haskell packages found.")
        sys.exit(0)

    os.makedirs("coverage", exist_ok=True)

    for cabal_file in target_cabal_files:
        suites = get_test_suites(cabal_file)
        if not suites:
            print(f"No test suites found in {cabal_file}")
            continue

        for suite in suites:
            run_coverage(suite)

if __name__ == "__main__":
    main()
