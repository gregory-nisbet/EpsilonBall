#! /bin/bash

set -o pipefail

test -d ./src || exit 10

# 1. delete uninteresting lines
# 2. capitalize status words like "passed"
stack 'test' 2>&1 | awk '
/^$/           { next; }
/^\[\]$/       { next; }
/^\[\".*\"\]$/ { next; }

1 == 1 {
  print $0;
}
' | sed 's/passed/PASSED/'
