import argparse
from dataclasses import dataclass

import subprocess
from os import path


@dataclass
class ExecutionResult:
    output: str
    has_verdict: bool


class GoblintMultishotRunner:

    def __init__(self):
        self.current_path = path.dirname(__file__)
        self.goblint_executable_path = path.join(self.current_path, "goblint_bin")

        parser = argparse.ArgumentParser(
            description="""A wrapper around goblint enabling multishot runs
            for SV-COMP.
            All args apart from --conf are passed on.
            """
        )
        parser.add_argument("--conf", type=str,
                            help="A plaintext file containing paths to configs relativ to the conf dir")
        conf_args, self.other_args = parser.parse_known_args()
        print("conf args:", conf_args)
        print("Got other args:", self.other_args)

        self.configs = []
        if conf_args.conf:
            with open(conf_args.conf, "r") as conflist_file:
                self.configs = [path.join(self.current_path, "conf", c.strip())
                                for c in conflist_file.readlines()]

    def run_with_config(self, config_path):
        args = ["--conf", config_path] + self.other_args
        output = subprocess.check_output([self.goblint_executable_path, *args]
                                         ).decode("utf-8")
        return ExecutionResult(output, self.has_verdict(output))

    def run_without_config(self):
        subprocess.run([self.goblint_executable_path, *self.other_args])

    @staticmethod
    def has_verdict(output):
        return any("SV-COMP result: true" in line for line in output)

    def run(self):
        if not self.configs:
            self.run_without_config()
            return

        result = None
        for config in self.configs:
            result = self.run_with_config(config)
            if result.has_verdict:
                break
        print(result.output)


if __name__ == "__main__":
    multishot_runner = GoblintMultishotRunner()
    multishot_runner.run()

