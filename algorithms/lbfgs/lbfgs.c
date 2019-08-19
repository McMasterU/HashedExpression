#include "problem.c"
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>

#define oo 10000000

extern const int var_size[NUM_VARIABLES];
extern const int var_offset[NUM_VARIABLES];
extern const int partial_derivative_offset[NUM_VARIABLES];
extern const int objective_offset;
extern double ptr[MEM_SIZE];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();

double random_in(double min, double max) {
  double range = (max - min);
  double div = RAND_MAX / range;
  return min + (rand() / div);
}

void print_vars() {
  int i, j;
  for (i = 0; i < NUM_VARIABLES; i++) {
    printf("var[%d] = [", i);
    for (j = 0; j < var_size[i]; j++) {
      printf("%f", ptr[var_offset[i] + j]);
      printf(j == var_size[i] - 1 ? "]\n" : ", ");
    }
  }
  printf("\n");
}

static lbfgsfloatval_t evaluate(void *instance, const lbfgsfloatval_t *x,
    lbfgsfloatval_t *g, const int n, const lbfgsfloatval_t step ) {
  int i, j;
  int cnt = 0;
  for (i = 0; i < NUM_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      ptr[var_offset[i] + j] = x[cnt];
      cnt++;
    }
  }
  evaluate_partial_derivatives_and_objective();
  cnt = 0;
  for (i = 0; i < NUM_VARIABLES; i++) {
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
  if (k % 1000 == 0) {
    printf("Iteration %d:\n", k);
    printf("fx = %f\n", fx);
  }
  return 0;
}

int main() {
  srand(time(NULL));
  int i, j;
  int ret = 0;

  int N = 0;
  for (i = 0; i < NUM_VARIABLES; i++) {
    N += var_size[i];
  }

  // assign values to predefined variables
  assign_values();

  lbfgsfloatval_t fx;
  lbfgsfloatval_t *x = lbfgs_malloc(N);
  lbfgs_parameter_t param;

  if (x == NULL) {
    printf("ERROR: Failed to allocate a memory block for variables.\n");
    return 1;
  }

  for (i = 0; i < N; i++) {
    x[i] = i;
  }

  lbfgs_parameter_init(&param);
  ret = lbfgs(N, x, &fx, evaluate, progress, NULL, &param);

  printf("f_min = %f at:\n", fx);
  for (i = 0; i < NUM_VARIABLES; i++) {
    printf("var[%d] = [", i);
    for (j = 0; j < var_size[i]; j++) {
      printf("%f", ptr[var_offset[i] + j]);
      printf(j == var_size[i] - 1 ? "]\n" : ", ");
    }
  }
  printf("\n");

  lbfgs_free(x);
  return 0;
}
