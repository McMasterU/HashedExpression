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
extern double ptr[MEM_SIZE];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();

int main() {
  // assign values to predefined variables (scalar, 1D, 2D, 3D)
  assign_values();
  printf("%d", NUM_VARIABLES);
  {
    int i;
    for (i = 0; i < var_size[0]; i++) {
      ptr[var_offset[0] + i] = 1;
    }
  }
  evaluate_partial_derivatives_and_objective();
  printf("%f\n", ptr[objective_offset]);
  {
    int i;
    for (i = 0; i < var_size[0]; i++) {
      printf("%f, ", ptr[partial_derivative_offset[0] + i]);
    }
  }
}