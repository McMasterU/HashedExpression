#!/bin/bash

should_compile() {
  if symphony "$1" > /dev/null; then
    echo -e "\e[32m$1"
  else
    echo -e "\e[31m$1: Should compiled but not"
  fi
}

should_not_compile() {
  if ! symphony "$1" > /dev/null; then
    echo -e "\e[32m$1"
  else
    echo -e "\e[31m$1: Should not compiled but compiled"
  fi
}

for file in correct/*.sp; do
  should_compile $file
done

for file in incorrect/*.sp; do
  should_not_compile $file
done


