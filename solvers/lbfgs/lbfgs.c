#include "problem.c"
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>
#include <string.h>

extern const char* var_name[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_num_dim[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_shape[NUM_HIGH_DIMENSIONAL_VARIABLES][3];
extern const int var_size[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int partial_derivative_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int objective_offset;

extern double ptr[MEMORY_NUM_DOUBLES];
extern complex double ptr_c[MEMORY_NUM_COMPLEX_DOUBLES];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();
extern void write_result();

int num_iterations;

double random_in(double min, double max) {
  double range = (max - min);
  double div = RAND_MAX / range;
  return min + (rand() / div);
}

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g, const int n, const lbfgsfloatval_t step ) {
  int i, j;
  evaluate_partial_derivatives_and_objective();
  int cnt = 0;
  for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      g[cnt] = ptr[partial_derivative_offset[i] + j];
      cnt++;
    }
  }

  return ptr[objective_offset];
}

static int progress(void *instance, const lbfgsfloatval_t *x,
    const lbfgsfloatval_t *g,
    const lbfgsfloatval_t fx,
    const lbfgsfloatval_t xnorm, const lbfgsfloatval_t gnorm, const lbfgsfloatval_t step, int n, int k,
    int ls) {
  num_iterations = k;
  if (k % 1000 == 0) {
    printf("Iteration %d:\n", k);
    printf("fx = %f\n", fx);
  }
  return 0;
}


int main() {
  srand(time(NULL));
  int i, j, k;
  int ret = 0;

  int N = 0;
  for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
    N += var_size[i];
  }

  // read values from file
  read_values();

  lbfgsfloatval_t fx;
  lbfgsfloatval_t *x = ptr + VARS_START_OFFSET;
  lbfgs_parameter_t param;

  // solve L-BFGS
  lbfgs_parameter_init(&param);
  ret = lbfgs(N, x, &fx, evaluate, progress, NULL, &param);

  printf("After %d iterations: \n", num_iterations);
  printf("f_min = %f\n", fx);
  write_result();
  printf("Done\n");

  lbfgs_free(x);
  return 0;
}
