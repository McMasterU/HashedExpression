#include "glpk.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include "problem.c"

/* problem.c Declarations */
extern const char* var_name[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_num_dim[NUM_HIGH_DIMENSIONAL_VARIABLES];
/* extern const int var_shape[NUM_HIGH_DIMENSIONAL_VARIABLES][3]; */
extern const int var_size[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int partial_derivative_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int objective_offset;

extern double ptr[MEMORY_NUM_DOUBLES];
extern complex double ptr_c[MEMORY_NUM_COMPLEX_DOUBLES];

extern const int bound_pos[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern double lower_bound[NUM_VARIABLES];
extern double upper_bound[NUM_VARIABLES];

extern double sc_lower_bound[NUM_GENERAL_CONSTRAINT];
extern double sc_upper_bound[NUM_GENERAL_CONSTRAINT];
extern const int sc_offset[NUM_GENERAL_CONSTRAINT];

extern const int sc_partial_derivative_offset[NUM_GENERAL_CONSTRAINT][NUM_HIGH_DIMENSIONAL_VARIABLES];

extern void read_values();
extern void read_bounds();
extern void evaluate_partial_derivatives_and_objective();
extern void evaluate_objective();
extern void evaluate_partial_derivatives();
extern void evaluate_scalar_constraints();
extern void evaluate_scalar_constraints_jacobian();
extern void write_result();

int main() {
  printf("Initializing...\n");
  glp_prob *lp;
  int n = NUM_VARIABLES, m = NUM_GENERAL_CONSTRAINT;
  int ia[10+(n*m)], ja[10+(n*m)];
  double ar[10+(n*m)];
  read_bounds();
  lp = glp_create_prob();
  glp_set_prob_name(lp, "glpk");
  glp_set_obj_dir(lp, GLP_MIN); // Assume minimization problem

  // Setup row constraints   --- (1)
  printf("Setting up row bounds = %d rows\n", NUM_GENERAL_CONSTRAINT);
  glp_add_rows(lp, NUM_GENERAL_CONSTRAINT);
  for (int i = 0; i < NUM_GENERAL_CONSTRAINT; i++) {
    glp_set_row_bnds(lp, i+1, GLP_DB, sc_lower_bound[i], sc_upper_bound[i]);
    printf("Bound on %d: %lf <= %d <= %lf\n", i, sc_lower_bound[i], i, sc_upper_bound[i]);
  }

  // Setup column constraints and coefficients --- (2)
  printf("Setting up column bounds = %d cols\n", NUM_VARIABLES);
  glp_add_cols(lp, NUM_VARIABLES);
  evaluate_objective();
  for (int i = 0; i < NUM_VARIABLES; i++) {
    glp_set_col_bnds(lp, i+1, GLP_DB, lower_bound[i], upper_bound[i]);
    glp_set_obj_coef(lp, i+1, ptr[partial_derivative_offset[i]]); // Is this in order..
    printf("%s coeff: %lf\n", var_name[i], ptr[partial_derivative_offset[i]]);
    printf("bounds: %lf <= %s <= %lf\n", lower_bound[i], var_name[i], upper_bound[i]);
  }

  // Setup matrix  --- (3)
  // evaluate_scalar_constraints or evaluate_scalar_constraints_jacobian?
  printf("Setting up constraint matrix\n");
  evaluate_scalar_constraints_jacobian();
  for (int i = 0; i < NUM_GENERAL_CONSTRAINT; i++) {
    for (int j = 0; j < NUM_VARIABLES; j++) {
      // Note: glpk arrays are indexed starting from 1
      int index = (i*NUM_VARIABLES) + (j+1);
      ia[index] = i+1, ja[index] = j+1,
      ar[index] = ptr[sc_partial_derivative_offset[i][j]];
      printf("index(%d) (%d, %d): %lf\n", index, ia[index], ja[index], ar[index]);
    }
  }

  for (int i = 0; i < n*m; i++) {
    printf("%d: ia = %d, ja = %d\n", i+1, ia[i+1], ja[i+1]);
  }

  // Load and solve
  glp_load_matrix(lp, n*m, ia, ja, ar);
  int exit_code = glp_simplex(lp, NULL);


  // Extract information from solver
  if (exit_code == 0) {
    // Starting point
    double *x = ptr + VARS_START_OFFSET;

    for (int i = 0; i < NUM_VARIABLES; i++) {
      x[i] = glp_get_col_prim(lp, i+1);
    }

    write_result();
  }
  else {
    printf("\n\nERROR OCCURED DURING GLPK OPTIMIZATION.\n");
  }

  // cleanup
  glp_delete_prob(lp);
  return exit_code;
}

/*
Each numbered section corresponds to a code block above
Example LP:   --- (2)
Min      25*x1 + 15*x2

Constraints: --- (3)
     p = 50*x1 + 70*x2
     q = 40*x1 + 20*x2
     r = 20*x1 + 10*x2

Bounds:
    --- (1)           ---- (2)
     p >= 170         0 <= x1
     q >= 250         0 <= x2
     r >= 60
*/
