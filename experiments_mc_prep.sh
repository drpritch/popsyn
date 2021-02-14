#!/bin/sh
while [ true ]; do
    # Just keep calling this until someone stops us.
    echo "source('experiments_mc_prep.R')" | R --no-save
done
