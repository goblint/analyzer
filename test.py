# import fileinput
# for line in fileinput.input():
#     pass

import sys
import re

if len(sys.argv) != 2:
	print "Stdin: output from goblint, 1. argument: C source-file"
	sys.exit(1)
path = sys.argv[1]

goblint = {}
for line in sys.stdin.readlines():
	m = re.match(r"(.+) \("+re.escape(path)+":(.+)\)", line)
	if m: goblint[m.group(2)] = m.group(1)

source = {}
lines = open(path).readlines()
for i,line in zip(range(1, len(lines)+1), lines):
	m = re.match(r".+ // Warn here: (.+)", line)
	if m: source[i] = m.group(1)

print "#"*50
print path

print "## Goblint warnings:"
for k,v in goblint.iteritems():
	print k, "\t", v
print

print "## Source warnings:"
for k,v in source.iteritems():
	print k, "\t", v
print

print "## Diff:"


print