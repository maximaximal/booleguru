#!/usr/bin/env bash

for d in *.dot; do
    echo "$(basename $d .dot).png"
    dot $d -Tpng -o $(basename $d .dot).png;
    rm $d;
done
