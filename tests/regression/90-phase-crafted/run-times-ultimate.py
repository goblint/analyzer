#!/usr/bin/env python3
"""Benchmark Goblint and Ultimate with and without witnesses on all 50 tasks."""

import argparse
import csv
import os
import re
import shlex
import shutil
import signal
import subprocess
import sys
import time
from pathlib import Path


def run_command(command, cwd, log, timeout):
    # Ultimate.py launches Java; stop the whole process group on timeout.
    with subprocess.Popen(command, cwd=cwd, stdout=log, stderr=subprocess.STDOUT,
                          start_new_session=True) as process:
        try:
            return process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
            raise


def main():
    suite = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--goblint", default=str(suite.parents[2] / "goblint"),
                        help="Goblint executable (default: repository's goblint)")
    parser.add_argument("--ultimate-dir", type=Path,
                        default=Path("/home/michael/Documents/papers/validating-concurrent-correctness-witnesses/ultimate/v.0.3.2/UGemCutter-linux"),
                        help="Ultimate installation directory containing Ultimate.py (default: %(default)s)")
    parser.add_argument("--bitwuzla", type=Path,
                        help="Bitwuzla executable (default: repository's result/tools/bitwuzla-0.9.1 if present, otherwise Ultimate's bundled solver)")
    parser.add_argument("--spec", type=Path,
                        default=suite.parents[2] / "scripts/sv-comp/smoketests/unreach-call.prp",
                        help="SV-COMP property file (default: unreach-call.prp)")
    parser.add_argument("--output", type=Path, default=suite / "result" / "runtimes-ultimate",
                        help="directory for CSV, Markdown and per-run logs")
    parser.add_argument("--timeout", type=float, default=300,
                        help="wall-clock limit per run in seconds (default: 300)")
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    executable = shutil.which(args.goblint)
    if executable is None:
        parser.error(f"Goblint executable not found: {args.goblint}")
    executable = str(Path(executable).resolve())
    ultimate = args.ultimate_dir.expanduser().resolve()
    spec = args.spec.expanduser().resolve()
    for path in (ultimate / "Ultimate.py", spec):
        if not path.is_file():
            parser.error(f"File not found: {path}")

    local_bitwuzla = suite.parents[2] / "result/tools/bitwuzla-0.9.1"
    bitwuzla = (args.bitwuzla or (local_bitwuzla if local_bitwuzla.is_file()
                                else ultimate / "bitwuzla")).expanduser().resolve()
    try:
        version = subprocess.run([str(bitwuzla), "--version"], capture_output=True,
                                 text=True, timeout=10, check=True)
    except (OSError, subprocess.TimeoutExpired, subprocess.CalledProcessError) as error:
        detail = error.stderr.strip() if isinstance(error, subprocess.CalledProcessError) else str(error)
        parser.error(f"Cannot run Bitwuzla at {bitwuzla}: {detail}. Use --bitwuzla PATH to select a compatible build.")

    output = args.output.resolve()
    ultimate_cwd = output / "ultimate-work"
    ultimate_cwd.mkdir(parents=True, exist_ok=True)
    # Ultimate.py prepends its installation to PATH, but Ultimate's Java solver
    # lookup checks the working directory first, including for the FOX strategy.
    solver_link = ultimate_cwd / "bitwuzla"
    if solver_link.is_symlink():
        solver_link.unlink()
    solver_link.symlink_to(bitwuzla)
    print(f"Using Bitwuzla {version.stdout.strip()}: {bitwuzla}", flush=True)

    # Reuse the regression commands, including task-specific invariant settings.
    tasks = []
    for source in sorted(suite.glob("[0-9][0-9]-*.c")):
        commands = []
        for line in source.with_suffix(".t").read_text().splitlines():
            if line.startswith("  $ goblint "):
                command, separator, _ = line[4:].partition(" 2>&1 |")
                if not separator:
                    parser.error(f"Unexpected command format in {source.stem}.t")
                commands.append([executable, *shlex.split(command)[1:]])
        if len(commands) != 2 or "witness.yaml.validate" in commands[0] or "witness.yaml.validate" not in commands[1]:
            parser.error(f"Expected baseline and witness commands in {source.stem}.t")
        witness = (suite / commands[1][commands[1].index("witness.yaml.validate") + 1]).resolve()
        if not witness.is_file():
            parser.error(f"File not found: {witness}")
        # The suite and its LP64 witnesses use 64-bit semantics.
        ultimate_command = [
            sys.executable, str(ultimate / "Ultimate.py"),
            "--spec", str(spec), "--file", str(source),
            "--full-output", "--architecture", "64bit",
            "--witness-dir", str(ultimate_cwd),
        ]
        ultimate_witness_command = [
            *ultimate_command, "--witness-type", "correctness_witness",
            "--witnessparser.only.consider.loop.invariants", "false",
            "--validate", str(witness),
        ]
        tasks.append((source.stem, [
            ("goblint.baseline", "unknown", commands[0], suite),
            ("goblint.witness", "true", commands[1], suite),
            ("ultimate.baseline", "true", ultimate_command, ultimate_cwd),
            ("ultimate.witness", "true", ultimate_witness_command, ultimate_cwd),
        ]))
    if len(tasks) != 50:
        parser.error(f"Expected 50 tasks, found {len(tasks)}")

    failed = False
    header = ("| Task | Goblint baseline (s) | Result | Goblint witness (s) | Result | "
              "Ultimate baseline (s) | Result | Ultimate witness (s) | Result |")
    rule = "| --- |" + " ---: | --- |" * 4
    with (output / "runtimes.csv").open("w", newline="") as csv_file, \
            (output / "runtimes.md").open("w") as table:
        writer = csv.writer(csv_file)
        writer.writerow(["task", "goblint_baseline_seconds", "goblint_baseline_result",
                         "goblint_witness_seconds", "goblint_witness_result",
                         "ultimate_baseline_seconds", "ultimate_baseline_result",
                         "ultimate_witness_seconds", "ultimate_witness_result"])
        print(header, rule, sep="\n", flush=True)
        table.write(header + "\n" + rule + "\n")
        for task, commands in tasks:
            row = [task]
            for mode, expected, command, cwd in commands:
                log_path = output / f"{task}.{mode}.log"
                start = time.perf_counter()
                with log_path.open("w") as log:
                    try:
                        returncode = run_command(command, cwd, log, args.timeout)
                        status = None if returncode == 0 else f"exit {returncode}"
                    except subprocess.TimeoutExpired:
                        status = "timeout"
                    except OSError as error:
                        log.write(f"Could not run command: {error}\n")
                        status = "execution error"
                elapsed = time.perf_counter() - start
                if status is None:
                    pattern = (r"^Result:[ \t]*\r?\n([^\r\n]+)"
                               if mode.startswith("ultimate.")
                               else r"^SV-COMP result:\s*(\S+)")
                    matches = re.findall(pattern,
                                         log_path.read_text(errors="replace"), re.MULTILINE)
                    status = matches[-1].strip().lower() if matches else "missing result"
                failed |= status != expected
                row.extend([f"{elapsed:.3f}", status])
            writer.writerow(row)
            csv_file.flush()
            line = "| " + " | ".join(row) + " |"
            print(line, flush=True)
            table.write(line + "\n")
            table.flush()
    print(f"\nTables and logs: {output}", flush=True)
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
