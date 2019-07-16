#!/usr/bin/env bash
while  stack test --test-arguments '--match "/StructureSpec/Structure spec/"' --fast; do
    echo "sucess"
done
#while stack test --fast; do
#    echo
#done
