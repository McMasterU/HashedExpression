#include "problem.c"
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>
#include <string.h>

extern const char* var_name[NUM_VARIABLES];
extern const int var_num_dim[NUM_VARIABLES];
extern const int var_shape[NUM_VARIABLES][3];
extern const int var_size[NUM_VARIABLES];
extern const int var_offset[NUM_VARIABLES];
extern const int partial_derivative_offset[NUM_VARIABLES];
extern const int objective_offset;
extern double ptr[MEM_SIZE];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();

int num_iterations;

double random_in(double min, double max) {
  double range = (max - min);
  double div = RAND_MAX / range;
  return min + (rand() / div);
}

void print_vars() {
  int i;
  for (i = 0; i < NUM_VARIABLES; i++) {
    char* var_file_name = (char*) malloc(strlen(var_name[i]) + 7);
    strcpy(var_file_name, var_name[i]);
    strcat(var_file_name, "_out.h5");
    if (var_num_dim[i] == 0) {
      printf("%s = %lf\n", var_name[i], ptr[var_offset[i]]);
    } else {
      printf("Writing %s to %s...\n", var_name[i], var_file_name);
      hid_t file, space, dset;
      hsize_t dims[3] = { (hsize_t) var_shape[i][0], (hsize_t) var_shape[i][1], (hsize_t) var_shape[i][2]};
      file = H5Fcreate(var_file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
      space = H5Screate_simple (var_num_dim[i], dims, NULL);
      dset = H5Dcreate (file, var_name[i], H5T_IEEE_F64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      H5Dwrite(dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptr + var_offset[i]);
      H5Dclose(dset);
      H5Sclose(space);
      H5Fclose(file);
    }
    free(var_file_name);
  }
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
  for (i = 0; i < NUM_VARIABLES; i++) {
    N += var_size[i];
  }

  // read values from file
  read_values();

  lbfgsfloatval_t fx;
  lbfgsfloatval_t *x = lbfgs_malloc(N);
  lbfgs_parameter_t param;

  if (x == NULL) {
    printf("ERROR: Failed to allocate a memory block for variables.\n");
    return 1;
  }

  // copying initial values
  int cnt = 0;
  for (i = 0; i < NUM_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      x[cnt] = ptr[var_offset[i] + j];
      cnt++;
    }
  }

  // solve L-BFGS
  lbfgs_parameter_init(&param);
  ret = lbfgs(N, x, &fx, evaluate, progress, NULL, &param);

  printf("After %d iterations: \n", num_iterations);
  printf("f_min = %f\n", fx);
  print_vars();
  printf("Done\n");

  lbfgs_free(x);
  return 0;
}
