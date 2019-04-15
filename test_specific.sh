#!/bin/bash

TEST_FILTER=$1

echo "TEST_FILTER = $TEST_FILTER"
stack test --fast --ta "-p "$TEST_FILTER"" --interleaved-output