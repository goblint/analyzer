#!/usr/bin/env python3
import argparse
from dataclasses import dataclass

import subprocess
from os import path
import logging


@dataclass
class ExecutionResult:
    output: str
    has_verdict: bool


class GoblintMultishotRunner:

    def __init__(self, logger):
        self.logger = logger
        self.current_path = path.dirname(__file__)
        self.goblint_executable_path = path.join(self.current_path, "goblint")
        if not path.exists(self.goblint_executable_path):
            logger.error(f" Could not find goblint executable at {self.goblint_executable_path}; did you build goblint with make?")
            exit(1)

        parser = argparse.ArgumentParser(
            description="""A facade in front of goblint enabling multishot runs for SV-COMP.
            All args apart from --multishot/-m are passed on to the actual goblint calls.
            The Multishotlist file is a plaintext file whose lines each consist of a path to a 
            goblint config file (relative to the goblint conf dir). 
            Goblint is run with each config in order until one produces a verdict true or reaches the end of the list.
            You may add comments to the multishotlist file by starting a line with #.
            """
        )
        parser.add_argument("-m","--multishotlist", type=str, metavar="FILE",dest="multishot",
                            help="a path to a multishotlist (relative to goblint_multishot.py)")
        conf_args, self.other_args = parser.parse_known_args()
        logger.debug(f"Multishot conf file: {conf_args.multishot}")
        logger.debug(f"Arguments passed on to goblint: {" ".join(self.other_args)}")

        self.configs = []
        if conf_args.multishot:
            if not path.exists(conf_args.multishot):
                logger.error(f" Could not find conf file at {conf_args.multishot}")
                exit(1)
            with open(conf_args.multishot, "r") as conflist_file:
                self.configs = [path.join(self.current_path, "conf", c.strip())
                                for c in conflist_file.readlines() if not c.strip().startswith("#")]
            logger.info(f"Loaded configs: {", ".join(self.configs)}")
    def run_with_config(self, config_path):
        args = ["--conf", config_path] + self.other_args
        self.logger.info(f"Running next shot: ./goblint {" ".join(args)}")
        output = subprocess.check_output([self.goblint_executable_path, *args]
                                        ).decode("utf-8")        

        return ExecutionResult(output, self.has_verdict(output))

    def run_without_config(self):
        subprocess.run([self.goblint_executable_path, *self.other_args])

    @staticmethod
    def has_verdict(output):
        return any("SV-COMP result: true" in line for line in output.splitlines())

    def run(self):
        if not self.configs:
            self.run_without_config()
            return

        result = None
        for config in self.configs:
            if not path.exists(config):
                logger.warning(f"Config file {config} not found, skipping.")
                continue
            if not path.isfile(config):
                logger.warning(f"Config file {config} is not a file, skipping.")
                continue
            result = self.run_with_config(config)
            if result.has_verdict:
                break
        print(result.output)


if __name__ == "__main__":
    logger=logging.getLogger("multishot")
    logging.basicConfig(level=logging.INFO)
    formatter=logging.Formatter('[%(levelname)s][%(name)s] %(message)s')
    sh=logging.StreamHandler()
    sh.setFormatter(formatter)
    logger.addHandler(sh)
    logger.propagate=False
    multishot_runner = GoblintMultishotRunner(logger)
    multishot_runner.run()