# Generate asserts for coreutil programs in ../bench/coreutils/
import os

outfile_setting="--set trans.output"
out_file_name="instrumented_"
outdir="goblint-coreutils/"
cmd ="./goblint  --set pre.cppflags[+] \"--std=gnu89\" --set ana.ctx_insens \"['base', 'mallocWrapper']\" --disable witness.invariant.full --set trans.activated[+] \"assert\" "
directory = "../bench/coreutils/"

header = """// This file is part of the SV-Benchmarks collection of verification tasks:
// https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks
// SPDX-FileCopyrightText: 1985-2011 Free Software Foundation, Inc.
// SPDX-FileCopyrightText: 2011-2022 University of Tartu & Technische Universität München
//
// SPDX-License-Identifier: GPL-3.0-only
"""

start_string = header + """#include <assert.h>
extern void abort(void);
void reach_error() { assert(0); }
#define __VERIFIER_assert(cond) { if(!(cond)) { reach_error(); abort(); } }
"""

def prepend(file, pre):
    f = open(file, "r")
    contents = f.read()
    f.close()

    f = open(file, "w")
    f.write(pre)
    f.write(contents)
    f.close()

def gernerat_yml(i_file):
    text = """format_version: '2.0'
input_files: '"""+ i_file + """'

properties:
  - property_file: ../properties/unreach-call.prp
    expected_verdict: true

options:
  language: C
  data_model: LP64
"""
    outfile = i_file.replace(".i", ".yml")
    outfile = outdir + outfile

    f = open(outfile, "w")
    f.write(text)
    f.close()

def run_configuration(config, additional_prefix):
    for f in os.listdir(directory):
        if not os.path.isdir(outdir):
            os.mkdir(outdir)
        if f.endswith(".c"):
            filename = os.fsdecode(f)
            infile = directory + f
            outfile = outdir + out_file_name + additional_prefix + filename
            command = cmd + " " + config + " " + infile + " " + outfile_setting + " " + outfile
            print("Executing command: " + command)
            os.system(command)
            if not os.path.exists(outfile):
                continue
            prepend(outfile, start_string)

            outfile_prep = outfile.replace(".c", ".i");
            preprocess = "gcc -m64 -P -E " + outfile + " > " + outfile_prep
            os.system(preprocess)
            prepend(outfile_prep, header)
            gernerat_yml(os.path.basename(outfile_prep))

run_configuration("", "")
run_configuration(" --enable ana.int.interval ", "interval_")
