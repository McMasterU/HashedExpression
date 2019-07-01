#!/usr/bin/env bash
while  stack test --test-arguments '--match "/SimplifyEval.ZeroCSpec/simplify & eval property for Zero C/prop_AddMultiply/"' --fast; do
    echo "sucess"
done
