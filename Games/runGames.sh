#!/bin/bash

for i in {1..100}
do
    cabal run > log${i}
done
