#include "problem.c"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

extern const int NUM_VARIABLES;
extern const int MEM_SIZE;
extern const int var_size[NUM_VARIABLES];
extern const int var_offset[NUM_VARIABLES];
extern const int partial_derivative_offset[NUM_VARIABLES];
extern const int objective_offset;
extern const double ptr[MEM_SIZE];

extern void assign_values();

int main() {
  // assign values to predefined variables (scalar, 1D, 2D, 3D)
  assign_values();
}