#!/usr/bin/env python3
import argparse
from dataclasses import dataclass

import subprocess
from os import path
import logging

class GoblintRunner:

    def __init__(self, logger):
        self.logger = logger
        self.current_path = path.dirname(__file__)
        self.goblint_executable_path = path.join(self.current_path, "goblint")
        if not path.exists(self.goblint_executable_path):
            self.goblint_executable_path=path.join(self.current_path, "..", "..", "goblint")
            if not path.exists(self.goblint_executable_path):
                logger.error(f" Could not find goblint executable neither at {self.current_path} nor at {path.dirname(self.goblint_executable_path)}; did you build goblint with make?")
                exit(1)

        parser = argparse.ArgumentParser(
            description="""A facade in front of goblint to enable running a portfolio of configurations for SV-COMP.
            All args apart from --portfolio-conf/-p are passed on to the actual goblint calls.
            The portfolio config file is a plaintext file whose lines each consist of goblint parameters, in particular including
            --conf followed by a path to a goblint config file (relative to the goblint base dir, or absolute).
            Goblint is run with each parameterset in order of specification as long as goblint produces an unknown verdict or reaches the end of the list.
            You may add comments to the portfolio config file by starting a line with #.
            """
        )
        parser.add_argument("-p","--portfolio-conf", type=str, metavar="FILE",dest="portfolio",
                            help="a path to a portfolio configuration file (relative to goblint_runner.py)")
        conf_args, self.other_args = parser.parse_known_args()
        logger.debug(f"Portfolio-conf file: {conf_args.portfolio}")
        logger.debug(f"Arguments passed on to goblint: {" ".join(self.other_args)}")

        self.configs = []
        if conf_args.portfolio:
            conf_args.portfolio = path.join(path.dirname(self.goblint_executable_path), conf_args.portfolio)
            if not path.exists(conf_args.portfolio):
                logger.error(f" Could not find portfolio conf file at {conf_args.portfolio}")
                exit(1)
            with open(conf_args.portfolio, "r") as conflist_file:
                self.configs = [c.strip() for c in conflist_file.readlines() if not c.strip().startswith("#")]
            logger.info(f"Loaded goblint configs: {", ".join(self.configs)}")

    def run_with_config(self, config_str):
        config_args = config_str.split(" ")
        args = [*config_args] + self.other_args
        self.logger.info(f"Config details: ./goblint {" ".join(args)}")
        process = subprocess.Popen([self.goblint_executable_path, *args],stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
        continue_portfolio = False
        verdict = None
        for line in process.stdout:
            decoded_line = line.decode("utf-8")
            print(decoded_line, end="")
            if decoded_line.startswith("SV-COMP result: "):
                # remove "SV-COMP result: " prefix and any trailing whitespace
                verdict = decoded_line[len("SV-COMP result: "):].strip()
                if verdict == "unknown":
                    continue_portfolio = continue_portfolio or decoded_line.startswith("SV-COMP result: unknown")
        process.wait()
        print(flush=True) # flush output from this run before logging about starting of the next run
        # handle the returncode:
        if process.returncode != 0 and not continue_portfolio:
            if process.returncode== -11:
                print("Segmentation fault (core dumped)")
                continue_portfolio = True
            self.logger.error(f"goblint exited with code {process.returncode}")
        return verdict,continue_portfolio

    def run_without_config(self):
        subprocess.run([self.goblint_executable_path, *self.other_args])

    def run(self):
        if not self.configs:
            self.run_without_config()
            return

        for i, config in enumerate(self.configs):
            logger.info(f"Starting config [{i}]")
            verdict, go_on = self.run_with_config(config)
            if not verdict:
                logger.error(f"No SV-COMP verdict produced by goblint for config [{i}]")
            if not go_on:
                logger.info(f"Stopping portfolio sequence with verdict [{verdict}] after config [{i}]")
                break
        if go_on:
            logger.info("Reached end of portfolio sequence without definitive verdict.")

class GoblintLikeFormatter(logging.Formatter):
    LEVEL_NAMES = {
        'DEBUG': 'Debug',
        'INFO': 'Info',
        'WARNING': 'Warning',
        'ERROR': 'Error',
        'CRITICAL': 'Critical',
    }

    def format(self, record):
        levelname = self.LEVEL_NAMES.get(record.levelname, record.levelname)
        record.levelname = levelname
        return super().format(record)

if __name__ == "__main__":
    logger=logging.getLogger("goblintrunner")
    logging.basicConfig(level=logging.INFO)
    formatter=GoblintLikeFormatter('[%(levelname)s][%(name)s] %(message)s')
    sh=logging.StreamHandler()
    sh.setFormatter(formatter)
    logger.addHandler(sh)
    logger.propagate=False
    goblint_runner = GoblintRunner(logger)
    goblint_runner.run()
