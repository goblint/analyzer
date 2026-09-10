#!/usr/bin/env python3
"""Run both configurations of all 50 tasks and tabulate wall-clock seconds."""

import argparse
import csv
import re
import shlex
import shutil
import subprocess
import time
from pathlib import Path


def main():
    suite = Path(__file__).resolve().parent
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--goblint", default=str(suite.parents[2] / "goblint"),
                        help="Goblint executable (default: repository's goblint)")
    parser.add_argument("--output", type=Path, default=suite / "result" / "runtimes",
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
        tasks.append((source.stem, commands))
    if len(tasks) != 50:
        parser.error(f"Expected 50 tasks, found {len(tasks)}")

    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    failed = False
    header = "| Task | Baseline (s) | Result | Witness (s) | Result |"
    rule = "| --- | ---: | --- | ---: | --- |"
    with (output / "runtimes.csv").open("w", newline="") as csv_file, \
            (output / "runtimes.md").open("w") as table:
        writer = csv.writer(csv_file)
        writer.writerow(["task", "baseline_seconds", "baseline_result",
                         "witness_seconds", "witness_result"])
        print(header, rule, sep="\n", flush=True)
        table.write(header + "\n" + rule + "\n")
        for task, commands in tasks:
            row = [task]
            for mode, expected, command in zip(
                    ("baseline", "witness"), ("unknown", "true"), commands):
                log_path = output / f"{task}.{mode}.log"
                start = time.perf_counter()
                with log_path.open("w") as log:
                    try:
                        process = subprocess.run(command, cwd=suite, stdout=log,
                                                 stderr=subprocess.STDOUT,
                                                 timeout=args.timeout)
                        status = None if process.returncode == 0 else f"exit {process.returncode}"
                    except subprocess.TimeoutExpired:
                        status = "timeout"
                elapsed = time.perf_counter() - start
                if status is None:
                    matches = re.findall(r"^SV-COMP result:\s*(\S+)",
                                         log_path.read_text(errors="replace"), re.MULTILINE)
                    status = matches[-1] if matches else "missing result"
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
