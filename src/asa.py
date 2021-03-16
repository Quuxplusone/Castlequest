#!/usr/bin/env python
import sys
for line in sys.stdin:
    print({'0': '\n', '-': '', ' ': ''}.get(line[0]), line[2:-1])
