#! /bin/bash

set -o pipefail

test -d ./src || exit 10

# TODO: better convention for temporary file
outfile=/tmp/testoutput.$$

if stack 'test' &> "$outfile"; then
  printf 'PASS\n'
else
  cat <"$outfile"
  # TODO: preserve exit status
  exit 10
fi
