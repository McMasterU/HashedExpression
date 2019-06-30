#!/usr/bin/env bash
while  stack test --test-arguments '--match "/SimplifyEval.ZeroRSpec/simplify eval 0/prop_Add/"' --fast; do
    echo "sucess"
done
