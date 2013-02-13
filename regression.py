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
    if m: goblint[int(m.group(2))] = m.group(1)

source = {}
lines = open(path).readlines()
for i,line in zip(range(1, len(lines)+1), lines):
    m = re.match(r".+ // Warn here: (.+)", line)
    if m: source[i] = m.group(1)

diff = {}; 
for k,v in sorted(set.union(set(goblint.items()), set(source.items()))):
    if k in diff: continue
    if k in goblint and k in source and goblint[k]!=source[k]:
        diff[k] = ('D', [goblint[k], source[k]])
    elif (k,v) in goblint.items() and (k,v) not in source.items():
        diff[k] = ('G', [goblint[k]])
    elif (k,v) not in goblint.items() and (k,v) in source.items():
        diff[k] = ('S', [source[k]])

print "#"*50
print path

if not len(diff):
    print
    sys.exit(0)

if len(goblint):
    print "## Goblint warnings:"
    for k,v in sorted(goblint.items()):
        print k, "\t", v
    print

if len(source):
    print "## Source warnings:"
    for k,v in source.items():
        print k, "\t", v
    print

if len(diff):
    print "## Diff (G..only goblint, S..only source, D..different):"
    for k,(s,v) in sorted(diff.items()):
        print s, k, "\t", v[0]
        for v in v[1:]: print "\t", v

print
sys.exit(1)