#!/usr/bin/env python3
import argparse
from dataclasses import dataclass

import subprocess
from os import path
import logging


@dataclass
class ExecutionResult:
    output: str
    verdict_true_or_error: bool


class GoblintMultishotRunner:

    def __init__(self, logger):
        self.logger = logger
        self.current_path = path.dirname(__file__)
        self.goblint_executable_path = path.join(self.current_path, "goblint")
        if not path.exists(self.goblint_executable_path):
            logger.error(f" Could not find goblint executable at {self.goblint_executable_path}; did you build goblint with make?")
            exit(1)

        parser = argparse.ArgumentParser(
            description="""A facade in front of goblint to enable running a portfolio of configurations for SV-COMP.
            All args apart from --portfolio-conf/-p are passed on to the actual goblint calls.
            The portfolio config file is a plaintext file whose lines each consist of a path to a
            goblint config file (relative to the goblint conf dir). 
            Goblint is run with each config in order until one produces a verdict true or reaches the end of the list.
            You may add comments to the portfolio config file by starting a line with #.
            """
        )
        parser.add_argument("-p","--portfolio-conf", type=str, metavar="FILE",dest="portfolio",
                            help="a path to a portfolio configuration file (relative to goblint_multishot.py)")
        conf_args, self.other_args = parser.parse_known_args()
        logger.debug(f"Portfolio-conf file: {conf_args.portfolio}")
        logger.debug(f"Arguments passed on to goblint: {" ".join(self.other_args)}")

        self.configs = []
        if conf_args.portfolio:
            if not path.exists(conf_args.portfolio):
                logger.error(f" Could not find portfolio conf file at {conf_args.portfolio}")
                exit(1)
            with open(conf_args.portfolio, "r") as conflist_file:
                self.configs = [path.join(self.current_path, "conf", c.strip())
                                for c in conflist_file.readlines() if not c.strip().startswith("#")]
            logger.info(f"Loaded goblint configs: {", ".join(self.configs)}")
            
    def run_with_config(self, config_path):
        args = ["--conf", config_path] + self.other_args
        self.logger.info(f"Running next shot: ./goblint {" ".join(args)}")
        process = subprocess.Popen([self.goblint_executable_path, *args],stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
        output = []
        for line in process.stdout:
            decoded_line = line.decode("utf-8")
            print(decoded_line, end="")
            output.append(decoded_line)
        process.wait()
        output = "".join(output)            
        return ExecutionResult(output, self.verdict_true_or_error(output))

    def run_without_config(self):
        subprocess.run([self.goblint_executable_path, *self.other_args])

    @staticmethod
    def verdict_true_or_error(output):
        return not any("SV-COMP result: unknown" in line for line in output.splitlines())

    def run(self):
        if not self.configs:
            self.run_without_config()
            return

        result = None
        for config in self.configs:
            result = self.run_with_config(config)
            if result.verdict_true_or_error:
                break

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
    logger=logging.getLogger("multishot")
    logging.basicConfig(level=logging.INFO)
    formatter=GoblintLikeFormatter('[%(levelname)s][%(name)s] %(message)s')
    sh=logging.StreamHandler()
    sh.setFormatter(formatter)
    logger.addHandler(sh)
    logger.propagate=False
    multishot_runner = GoblintMultishotRunner(logger)
    multishot_runner.run()
