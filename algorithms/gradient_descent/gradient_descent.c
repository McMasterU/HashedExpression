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


const double ALPHA = 0.01;
const int MAX_ITER = 100;
const double THRESHOLD = 1e-6;

int main() {
  int i, j, k;
  // assign values to predefined variables
  assign_values();
  // initial for optimizing variables
  for (i = 0; i < NUM_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      ptr[var_offset[i] + j] = 0;
    }
  }

  int iter = 0;
  while (iter < MAX_ITER) {
    evaluate_partial_derivatives_and_objective();
    printf("f = %f\n", ptr[objective_offset]);
    double max_step = 0;
    for (i = 0; i < NUM_VARIABLES; i++) {
      for (j = 0; j < var_size[i]; j++) {
        double step = (-ALPHA) * ptr[partial_derivative_offset[i] + j];
        ptr[var_offset[i] + j] += step;
        max_step = fmax(max_step, fabs(step));
      }
    }

    if (max_step < THRESHOLD) break;
  }

  printf("f_min = %f at:\n", ptr[objective_offset]);
  for (i = 0; i < NUM_VARIABLES; i++) {
    printf("var[%d] = [", i);
    for (j = 0; j < var_size[i]; j++) {
      printf("%f", ptr[var_offset[i] + j]);
      printf(j == var_size[i] - 1 ? "]\n" : ", ");
    }
  }
}