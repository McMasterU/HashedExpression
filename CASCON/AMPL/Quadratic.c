#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.c"
#include "hdf5.h"

#define MEM_SIZE 10

#define VARS_START_OFFSET 5
#define NUM_VARIABLES 1
#define NUM_ACTUAL_VARIABLES 1

const char* var_name[NUM_VARIABLES] = {"x"};
const int var_num_dim[NUM_VARIABLES] = {0};
const int var_shape[NUM_VARIABLES][3] = {{1, 1, 1}};
const int var_size[NUM_VARIABLES] = {1};
const int var_offset[NUM_VARIABLES] = {0};
const int partial_derivative_offset[NUM_VARIABLES] = {7};
const int objective_offset = 6;
double ptr[MEM_SIZE];


const int bound_pos[NUM_VARIABLES] = {0};
double lower_bound[NUM_ACTUAL_VARIABLES];
double upper_bound[NUM_ACTUAL_VARIABLES];

#define NUM_SCALAR_CONSTRAINT 1

double sc_lower_bound[NUM_SCALAR_CONSTRAINT];
double sc_upper_bound[NUM_SCALAR_CONSTRAINT];
const int sc_offset[NUM_SCALAR_CONSTRAINT] = {8};

const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {{10}};

#define A 0
#define B 1
#define C 2
#define N 3
#define M 4
#define X 5
#define OBJ 6
#define DERIVATIVE 7
#define CONSTRAINT 8
#define CONSTRAINT_DERIVATIVE 9

void read_bounds() {
  lower_bound[0] = ptr[N];
  upper_bound[0] = INFINITY;
  sc_lower_bound[0] = -INFINITY;
  sc_upper_bound[0] = ptr[M];
}

void read_values() {
  ptr[A] = 1;
  ptr[B] = 0;
  ptr[C] = 5;
  ptr[N] = -10;
  ptr[M] = 0;
  ptr[X]= 0;
}

void evaluate_partial_derivatives_and_objective() {
  ptr[OBJ] = ptr[A] * ptr[X] * ptr[X] + ptr[B] * ptr[X] + ptr[C];
  ptr[DERIVATIVE] = 2.0 * ptr[A] * ptr[X] + ptr[B];
}

void evaluate_objective() {
  ptr[OBJ] = ptr[A] * ptr[X] * ptr[X] + ptr[B] * ptr[X] + ptr[C];
}

void evaluate_partial_derivatives() {
  ptr[DERIVATIVE] = 2.0 * ptr[A] * ptr[X] + ptr[B];
}

void evaluate_scalar_constraints() {
  ptr[CONSTRAINT] = pow(ptr[X],2);
}

void evaluate_scalar_constraints_jacobian() {
  ptr[CONSTRAINT_DERIVATIVE] = 2.0 * ptr[X];
}
