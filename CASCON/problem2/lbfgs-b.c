#include "problem.c"
#include "lbfgsb.h"
#include <time.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>
#include <string.h>

#define M 10

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

int main() {
    int i, j;
    // Helpers, DON'T TOUCH
    static double dsave[29];
    static integer isave[44];
    static logical lsave[4];
    static integer taskValue;
    static integer *task=&taskValue;
    static integer csaveValue;
    static integer *csave=&csaveValue;

    // f - objective, g - gradient
    static double f, g[NUM_ACTUAL_VARIABLES];
    static double* x = ptr + VARS_START_OFFSET;
    // l - lower bound, u - upper bound
    // nbd(i)= 0 if x(i) is unbounded,
    //         1 if x(i) has only a lower bound,
    //         2 if x(i) has both lower and upper bounds, and
    //         3 if x(i) has only an upper bound.
    static double l[NUM_ACTUAL_VARIABLES], u[NUM_ACTUAL_VARIABLES];
    static integer nbd[NUM_ACTUAL_VARIABLES];

    // m - number of gradient vectors use to approximate hessian
    // n - number of variables
    static integer m = M, n = NUM_ACTUAL_VARIABLES;

    // wa - working space
    // the size is at least (2mmax + 5)nmax + 12mmax^2 + 12mmax.
    const int WORKING_SPACE = (2 * M + 5) * (NUM_ACTUAL_VARIABLES + 1) + 12 * M * M + 12 * M;
    static double wa[WORKING_SPACE + 5];

    // iwa is an INTEGER  array of length 3nmax used as
    //   workspace. DON'T TOUCH
    static integer iwa[3 * NUM_ACTUAL_VARIABLES + 5];


    // MARK - STOPPING CRITERIA
    // factr is a double precision variable.
    //   On entry factr > 0 is specified by the user.  The iteration
    //     will stop when
    //
    //     (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
    //
    //     where epsmch is the machine precision, which is automatically
    //     generated by the code. Typical values for factr: 1.d+12 for
    //     low accuracy; 1.d+7 for moderate accuracy; 1.d+1 for extremely
    //     high accuracy.
    //   On exit factr is unchanged.
    static double factr = 1e7;

    // pgtol is a double precision variable.
    //   On entry pgtol > 0 is specified by the user.  The iteration
    //     will stop when
    //
    //             max{|proj g_i | i = 1, ..., n} <= pgtol
    //
    //     where pg_i is the ith component of the projected gradient.
    //   On exit pgtol is unchanged.
    static double pgtol = 1e-5;

    // iprint is an integer variable that must be set by the user.
    //   It controls the frequency and type of output generated:
    //    iprint<0    no output is generated;
    //    iprint=0    print only one line at the last iteration;
    //    0<iprint<99 print also f and |proj g| every iprint iterations;
    //    iprint=99   print details of every iteration except n-vectors;
    //    iprint=100  print also the changes of active set and final x;
    //    iprint>100  print details of every iteration including x and g;
    //   When iprint > 0, the file iterate.dat will be created to
    //                    summarize the iteration.
    static integer iprint = 0;


    // MARK: initialization, read values, bounds
    read_values();
    read_bounds();

    // bounds
    for (i = 0; i < NUM_ACTUAL_VARIABLES; i++) {
      l[i] = lower_bound[i];
      u[i] = upper_bound[i];
      if (lower_bound[i] == -INFINITY && upper_bound[i] == INFINITY) {
        nbd[i] = 0; // unbound
      } else if (lower_bound[i] != -INFINITY && upper_bound[i] == INFINITY) {
        nbd[i] = 1; // 1 if x(i) has only a lower bound,
      } else if (lower_bound[i] != -INFINITY && upper_bound[i] != INFINITY) {
        nbd[i] = 2; // bounded both
      } else if (lower_bound[i] == -INFINITY && upper_bound[i] != INFINITY) {
        nbd[i] = 3; // only lower_bound
      }
    }

    *task = START;
    /*        ------- the beginning of the loop ---------- */
L111:
    /*     This is the call to the L-BFGS-B code. */
    setulb(&n, &m, x, l, u, nbd, &f, g, &factr, &pgtol, wa, iwa, task, &
            iprint, csave, lsave, isave, dsave);
    if (IS_FG(*task)) {
        /*        the minimization routine has returned to request the */
        /*        function f and gradient g values at the current x. */
        /*        Compute function value f for the sample problem. */
        /* Computing 2nd power */
        /*          go back to the minimization routine. */

        evaluate_partial_derivatives_and_objective();

        int cnt = 0;
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            g[cnt] = ptr[partial_derivative_offset[i] + j];
            cnt++;
          }
        }

        f = ptr[objective_offset];

        goto L111;
    }

    if ( *task==NEW_X ) {
        goto L111;
    }

    print_vars();
    /*           ---------- the end of the loop ------------- */
    /*     If task is neither FG nor NEW_X we terminate execution. */
    return 0;
}
