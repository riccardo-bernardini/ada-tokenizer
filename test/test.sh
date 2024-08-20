#!/usr/bin/bash

if gprbuild -q test_tokenizer ; then
    # Nothing to do
    :
else
    echo "Could not build test_tokenizer. Exiting"
    exit 1
fi

#
# Array with testing case.  Every entry contains
#
# 1. The "mode"
# 2. The input
# 3. The expected output
#
# The three fields are separated by '%'
#
cases=('n%foo  bar%[foo] [] [bar]'
       'f%foo  bar%[foo] [bar]'
       'f%foo,,bar%[foo] [bar]'
       'n%foo,,bar%[foo] [] [bar]'
      )

for this in "${cases[@]}" ; do
    IFS='%' read mode input expected <<< "$this"

    output=`obj/test_tokenizer -q $mode "$input"`

    if [ "$output" = "$expected" ]; then
	echo PASSED
    else
	echo "FAILED [input: $mode $input/output:$output/expected:$expected]"
    fi
done
