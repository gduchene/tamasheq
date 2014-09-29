#!/bin/env bash

tmp=$(mktemp -d "/tmp/tamasheq.XXXX")
failure_count=0

cd tests

for input in *.ml; do
    expected="${input/ml/expected}"
    name="${input%\.ml}"
    output="$tmp/${input/ml/output}"

    if ../tamasheq -denv -o "$tmp" "$input" > "$output" 2>&1; then
        if cmp -s "$expected" "$output"; then
            printf "%-20s\tSUCCESS\n" "$name" >&2
            rm "$tmp/$name"*
        else
            printf "%-20s\tFAILURE\n" "$name" >&2
            failure_count=$((failure_count + 1))
        fi
    else
        printf "%-20s\tFAILURE\n" "$name" >&2
        failure_count=$((failure_count + 1))
    fi
done

if [ "$failure_count" -gt 0 ]; then
    printf "\n$failure_count test(s) failed\nplease see $tmp for more information\n" >&2
    exit 1
else
    printf "\nall the tests were successful\n" >&2
    rm -r "$tmp"
    exit 0
fi
