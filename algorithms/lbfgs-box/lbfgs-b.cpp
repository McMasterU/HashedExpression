#include "problem.c"
#include <time.h>
#include <math.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>
#include <string.h>
#include "cppoptlib/meta.h"
#include "cppoptlib/boundedproblem.h"
#include "cppoptlib/solver/lbfgsbsolver.h"

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
extern void evaluate_objective();
extern void evaluate_partial_derivatives();

int num_iterations;

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

using namespace cppoptlib;

class OptimizationProblem : public BoundedProblem<double> {
  public:
    using typename Problem<double>::TVector;
    double value(const TVector &x) {
      int cnt = 0;
      for (int i = 0; i < NUM_VARIABLES; i++) {
        for (int j = 0; j < var_size[i]; j++) {
          ptr[var_offset[i] + j] = x(cnt);
          cnt++;
        }
      }

      evaluate_objective();
      return ptr[objective_offset];
    }
    void gradient(const TVector &x, TVector &grad) {
      int cnt = 0;
      for (int i = 0; i < NUM_VARIABLES; i++) {
        for (int j = 0; j < var_size[i]; j++) {
          ptr[var_offset[i] + j] = x(cnt);
          cnt++;
        }
      }

      evaluate_partial_derivatives();
      cnt = 0;
      for (int i = 0; i < NUM_VARIABLES; i++) {
        for (int j = 0; j < var_size[i]; j++) {
          grad(cnt) = ptr[partial_derivative_offset[i] + j];
          cnt++;
        }
      }
    }
};

typedef typename OptimizationProblem::TVector TVector;

int main() {

  TVector x = TVector::Zero(NUM_ACTUAL_VARIABLES);
  TVector x_lower_bound = TVector::Zero(NUM_ACTUAL_VARIABLES);
  TVector x_upper_bound = TVector::Zero(NUM_ACTUAL_VARIABLES);

  // read fixed value, initialial values and bounds
  read_values();
  read_bounds();
  // copying initial values
  int cnt = 0;
  for (int i = 0; i < NUM_VARIABLES; i++) {
    for (int j = 0; j < var_size[i]; j++) {
      x(cnt) = ptr[var_offset[i] + j];
      cnt++;
    }
  }

  // copying lower bound and upper bound
  for (int i = 0; i < NUM_VARIABLES; i++) {
    x_lower_bound(i) = lower_bound[i];
    x_upper_bound(i) = upper_bound[i];
  }

  // optimization problem
  OptimizationProblem f;
  f.setLowerBound(x_lower_bound);
  f.setUpperBound(x_upper_bound);

  LbfgsbSolver<OptimizationProblem> solver;
  solver.minimize(f, x);

  cnt = 0;
  for (int i = 0; i < NUM_VARIABLES; i++) {
    for (int j = 0; j < var_size[i]; j++) {
      ptr[var_offset[i] + j] = x(cnt);
      cnt++;
    }
  }
}
