#!/usr/bin/env bash

for d in *.dot; do
    dot $d -Tpng -o $(basename $d .dot).png;
done
