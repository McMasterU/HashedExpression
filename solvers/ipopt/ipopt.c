/* Copyright (C) 2019
 * All Rights Reserved.
 * This code is published under the GNU GPL.
 *
 * Authors:  Curtis D'Alves
 */

#include "IpStdCInterface.h"
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

/* Ipopt Function Declarations */
Bool eval_f(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number*     obj_value,
   UserDataPtr user_data
);

Bool eval_grad_f(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number*     grad_f,
   UserDataPtr user_data
);

Bool eval_g(
   Index       n,
   Number*     x,
   Bool        new_x,
   Index       m,
   Number*     g,
   UserDataPtr user_data
);

Bool eval_jac_g(
   Index       n,
   Number*     x,
   Bool        new_x,
   Index       m,
   Index       nele_jac,
   Index*      iRow,
   Index*      jCol,
   Number*     values,
   UserDataPtr user_data
);

Bool eval_h(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number      obj_factor,
   Index       m,
   Number*     lambda,
   Bool        new_lambda,
   Index       nele_hess,
   Index*      iRow,
   Index*      jCol,
   Number*     values,
   UserDataPtr user_data
);

// TODO delete me?
Bool intermediate_cb(
   Index       alg_mod,
   Index       iter_count,
   Number      obj_value,
   Number      inf_pr,
   Number      inf_du,
   Number      mu,
   Number      d_norm,
   Number      regularization_size,
   Number      alpha_du,
   Number      alpha_pr,
   Index       ls_trials,
   UserDataPtr user_data
);

/** Main Program */
/* [MAIN] */
int main()
{
  Index n = -1;                        /* number of variables */
  Index m = -1;                        /* number of constraints */
  Index nele_jac;                      /* number of nonzeros in the Jacobian of the constraints */
  Index nele_hess;                     /* number of nonzeros in the Hessian of the Lagrangian (lower or upper triangular part only) */
  Index index_style;                   /* indexing style for matrices */
  // Number* x_L = NULL;                  /* lower bounds on x */
  // Number* x_U = NULL;                  /* upper bounds on x */
  // Number* g_L = NULL;                  /* lower bounds on g */
  // Number* g_U = NULL;                  /* upper bounds on g */
  IpoptProblem nlp = NULL;             /* IpoptProblem */
  enum ApplicationReturnStatus status; /* Solve return code */
  Number* x = NULL;                    /* starting point and solution vector */
  Number* mult_g = NULL;               /* constraint multipliers at the solution */
  Number* mult_x_L = NULL;             /* lower bound multipliers at the solution */
  Number* mult_x_U = NULL;             /* upper bound multipliers at the solution */
  Number obj;                          /* objective value */
  Index i;                             /* generic counter */
  // TODO remove unnecessary declerations above

  /* set number of variables and constraints */
   n = NUM_VARIABLES; m = NUM_GENERAL_CONSTRAINT;

   /* initializes bounds for variables and constraints
      (lower_bound,upper_bound,sc_lower_bound,sc_upper_bound) */
   read_bounds();

   /* set the number of nonzeros in the Jacobian and Hessian */
   nele_jac = NUM_VARIABLES*NUM_GENERAL_CONSTRAINT;  // TODO use sparse jacobian?
   nele_hess = NUM_VARIABLES*NUM_VARIABLES; // TODO use sparse hessian?

   /* set the indexing style to C-style (start counting of rows and column indices at 0) */
   index_style = 0;

   /* create the IpoptProblem */
   nlp = CreateIpoptProblem(n, lower_bound, upper_bound,
                            m, sc_lower_bound, sc_upper_bound,
                            nele_jac, nele_hess, index_style,
                            &eval_f, &eval_g, &eval_grad_f,
                            &eval_jac_g, &eval_h);

   /* TODO if lower_bound,upper_bound,_sc_lower_bound and sc_upper_bound were dynamically allocated
    *      we could free them here
    */

   /* Set ipopt options.
    */
   AddIpoptNumOption(nlp, "tol", 1e-7);  // TODO adjust me?
   AddIpoptStrOption(nlp, "mu_strategy", "adaptive"); // TODO does this help?
   AddIpoptStrOption(nlp, "hessian_approximation","limited-memory"); // TODO add option to turn off/on?

   /* Initialize variables and parameters */
   read_values();

   /* starting point */
   x = ptr + VARS_START_OFFSET;

   /* allocate space to store the bound multipliers at the solution */
   mult_g = (Number*) malloc(sizeof(Number) * NUM_GENERAL_CONSTRAINT);
   mult_x_L = (Number*) malloc(sizeof(Number) * NUM_VARIABLES);
   mult_x_U = (Number*) malloc(sizeof(Number) * NUM_VARIABLES);

   /* Set the callback method for intermediate user-control.
    * This is not required, just gives you some intermediate control in
    * case you need it.
    * TODO decide whether or not this is of use
    */
   /* SetIntermediateCallback(nlp, intermediate_cb); */

   /* solve the problem */
   status = IpoptSolve(nlp, x, NULL, &obj, mult_g, mult_x_L, mult_x_U, NULL);

   /* check IpoptSolve results and print solutions upon success
    * TODO write solution to file
    */
   if( status == Solve_Succeeded )
   {
      printf("\n\nSolution of the primal variables, x\n");
      for( i = 0; i < NUM_VARIABLES; i++ )
      {
         printf("x[%d] = %f\n", i, x[i]);
      }

      // printf("\n\nSolution of the constraint multipliers, lambda\n");
      // for( i = 0; i < NUM_GENERAL_CONSTRAINT; i++ )
      // {
      //    printf("lambda[%d] = %e\n", i, mult_g[i]);
      // }
      // printf("\n\nSolution of the bound multipliers, z_L and z_U\n");
      // for( i = 0; i < NUM_VARIABLES; i++ )
      // {
      //    printf("z_L[%d] = %e\n", i, mult_x_L[i]);
      // }
      // for( i = 0; i < NUM_VARIABLES; i++ )
      // {
      //    printf("z_U[%d] = %e\n", i, mult_x_U[i]);
      // }

      printf("\n\nObjective value\nf(x*) = %e\n", obj);
      write_result();
   }
   else
   {
      printf("\n\nERROR OCCURRED DURING IPOPT OPTIMIZATION.\n");
   }

   /* free allocated memory */
   FreeIpoptProblem(nlp);
   free(mult_g);
   free(mult_x_L);
   free(mult_x_U);

   return (status == Solve_Succeeded) ? EXIT_SUCCESS : EXIT_FAILURE;
}
/* [MAIN] */

/* Function Implementations */
Bool eval_f(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number*     obj_value,
   UserDataPtr _
)
{
  memcpy(ptr + VARS_START_OFFSET, x, sizeof(Number) * NUM_VARIABLES);
  evaluate_objective();
  *obj_value = ptr[objective_offset];

  return TRUE;
}

Bool eval_grad_f(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number*     grad_f,
   UserDataPtr _
)
{
  memcpy(ptr + VARS_START_OFFSET, x, sizeof(Number) * NUM_VARIABLES);
  evaluate_partial_derivatives();

  /* copy partial derivatives from shared memory into grad_f
   * NOTE: unfortunately because of hashing, we cannot guarentee partials will be stored
   *       sequentially in memory
   */
  int acc = 0; int i;
  for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
    memcpy(grad_f + acc, ptr + partial_derivative_offset[i], sizeof(Number) * var_size[i]);
    acc += var_size[i];
  }

  return TRUE;
}

Bool eval_g(
   Index       n,
   Number*     x,
   Bool        new_x,
   Index       m,
   Number*     g,
   UserDataPtr _
)
{
  memcpy(ptr + VARS_START_OFFSET, x, sizeof(Number) * NUM_VARIABLES);
  evaluate_scalar_constraints();

  // TODO scalar constraints are always ... scalars right?
  //      like each constraint always evaluates to a single double?
  int i;
  for (i = 0; i < NUM_GENERAL_CONSTRAINT; i++) {
    g[i] = ptr[sc_offset[i]];
  }
  return TRUE;
}

Bool eval_jac_g(
   Index       n,
   Number*     x,
   Bool        new_x,
   Index       m,
   Index       nele_jac,
   Index*      iRow,
   Index*      jCol,
   Number*     values,
   UserDataPtr _
)
{
   if( values == NULL )
   {
      /* return the structure of the jacobian OF THE CONSTRAINTS (not objective) */

      /* following speifies completely dense jacobian
       * TODO currently variables per column, constraint per row (is this correct?)
       */
     int i;
     for (i = 0; i < nele_jac; i++) {
       iRow[i] = i / NUM_VARIABLES;
       jCol[i] = i % NUM_VARIABLES;
     }
   }
   else
   {
     /* evaluate jacobian of the constraints */
     memcpy(ptr + VARS_START_OFFSET, x, sizeof(Number) * NUM_VARIABLES);
     evaluate_scalar_constraints_jacobian();

     /* return the values of the jacobian of the constraints */
     /* FIXME does accumulation ever cause a bad access? */
     int i, j, acc = 0;
     for (i = 0; i < NUM_GENERAL_CONSTRAINT; i++) {
       for (j = 0; j < NUM_HIGH_DIMENSIONAL_VARIABLES; j++) {
         memcpy(values + acc, ptr + sc_partial_derivative_offset[i][j], sizeof(Number) * var_size[j]);
         acc += var_size[j];
       }
     }
   }

   return TRUE;
}

Bool eval_h(
   Index       n,
   Number*     x,
   Bool        new_x,
   Number      obj_factor,
   Index       m,
   Number*     lambda,
   Bool        new_lambda,
   Index       nele_hess,
   Index*      iRow,
   Index*      jCol,
   Number*     values,
   UserDataPtr _
)
{
  /* no work is performed because hessian_approximation (lbfgs) option is on */
  /* TODO have option to actually return hessian when option is off */
  return FALSE;
}

Bool intermediate_cb(
   Index       alg_mod,
   Index       iter_count,
   Number      obj_value,
   Number      inf_pr,
   Number      inf_du,
   Number      mu,
   Number      d_norm,
   Number      regularization_size,
   Number      alpha_du,
   Number      alpha_pr,
   Index       ls_trials,
   UserDataPtr _
)
{
   printf("Testing intermediate callback in iteration %d\n", iter_count);
   if( inf_pr < 1e-4 )
   {
     return FALSE;
   }

   /* TODO remove this or actually use it? */
   return TRUE;
}
