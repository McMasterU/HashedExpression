#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "hdf5.h"
#include <complex.h>

%{fftUtils}

// number of (higher dimensional) variables 
#define NUM_VARIABLES %{numHigherOrderVariables}
// number of scalar variables (because each higher dimensional var is a grid of scalar variables)
#define NUM_ACTUAL_VARIABLES %{numActualVariables}
#define MEMORY_NUM_DOUBLES %{totalDoubles}
#define MEMORY_NUM_COMPLEX_DOUBLES %{totalComplexes}
// all the actual double variables are allocated one after another, starts from here
#define VARS_START_OFFSET %{varStartOffset}
#define MAX_NUM_ITERATIONS %{maxNumIterations}
#define NUM_SCALAR_CONSTRAINT %{numScalarConstraints}

const char* var_name[NUM_VARIABLES] = { %{varNames} };
const int var_size[NUM_VARIABLES] = { %{varSizes} };

const int var_offset[NUM_VARIABLES] = { %{varOffsets} };
const int partial_derivative_offset[NUM_VARIABLES] = { %{partialDerivativeOffsets} };
const int objective_offset = %{objectiveOffset};

double ptr[MEMORY_NUM_DOUBLES];
complex double ptr_c[MEMORY_NUM_COMPLEX_DOUBLES];

double lower_bound[NUM_ACTUAL_VARIABLES];
double upper_bound[NUM_ACTUAL_VARIABLES];

double sc_lower_bound[NUM_SCALAR_CONSTRAINT];
double sc_upper_bound[NUM_SCALAR_CONSTRAINT];
const int sc_offset[NUM_SCALAR_CONSTRAINT] = { %{scalarConstraintOffsets} };

const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {  %{scalarConstraintPartialDerivativeOffsets} };

void read_bounds() {
  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {
    lower_bound[i] = -INFINITY;
    upper_bound[i] = INFINITY;
  }
  %{readBounds}
  %{readBoundScalarConstraints}
}

void read_values() {
  srand(time(NULL));
  %{readValues}
}

void write_result() {
  %{writeResult}
}

void evaluate_partial_derivatives_and_objective() {
  %{evaluatePartialDerivativesAndObjective}

}
void evaluate_objective() {
  %{evaluateObjective}
}
void evaluate_partial_derivatives() {
  %{evaluatePartialDerivatives}
}
void evaluate_scalar_constraints() {
  %{evaluateScalarConstraints}
}
void evaluate_scalar_constraints_jacobian() {
  %{evaluateScalarConstraintsJacobian}
}