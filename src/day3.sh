#!/bin/bash

mmm="xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

pattern="mul\(([0-9]+),([0-9]+)\)"


if [[ "$mmm" =~ $pattern ]]; then
  all=${BASH_REMATCH[0]}
  domain=${BASH_REMATCH[1]}
  for i in "${!BASH_REMATCH[@]}"; do
    echo "$i: ${BASH_REMATCH[$i]}"
  done
else
    echo "Invalid URL"
fi