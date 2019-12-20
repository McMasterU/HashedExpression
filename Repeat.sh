#!/usr/bin/env bash
#while  stack test --test-arguments '--match "/HashedToCSpec/Hashed To C spec/Evaluate hash interp should equal to C code evaluation (Expression One R)/"' --fast; do
#    echo "sucess"
#done
while stack test --fast; do
    echo
done
