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
    integer i__1;
    /* Local variables */
    static double f, g[1024];
    static integer i__;
    static double l[1024];
    static integer m, n;
    static double u[1024], x[1024], wa[43251];
    static integer nbd[1024], iwa[3072];
/*     static char task[60]; */
    static integer taskValue;
    static integer *task=&taskValue; /* must initialize !! */
    static double factr;
/*     static char csave[60]     */
    static integer csaveValue;
    static integer *csave=&csaveValue;
    static double dsave[29];
    static integer isave[44];
    static logical lsave[4];
    static double pgtol;
    static integer iprint;
    /*
    This driver shows how to replace the default stopping test
      by other termination criteria. It also illustrates how to
      print the values of several parameters during the course of
      the iteration. The sample problem used here is the same as in
      DRIVER1 (the extended Rosenbrock function with bounds on the
      variables).
       nmax is the dimension of the largest problem to be solved.
       mmax is the maximum number of limited memory corrections.
    Declare the variables needed by the code.
      A description of all these variables is given at the end of
      driver1.
     */

    /*     Declare a few additional variables for the sample problem. */
    /*     We suppress the default output. */
    iprint = -1;
    /*     We suppress both code-supplied stopping tests because the */
    /*        user is providing his own stopping criteria. */
    factr = 0.;
    pgtol = 0.;
    /*     We specify the dimension n of the sample problem and the number */
    /*        m of limited memory corrections stored.  (n and m should not */
    /*        exceed the limits nmax and mmax respectively.) */
    n = 4;
    m = 5;
    /*     We now specify nbd which defines the bounds on the variables: */
    /*                    l   specifies the lower bounds, */
    /*                    u   specifies the upper bounds. */
    /*     First set bounds on the odd numbered variables. */
    for (i__ = 1; i__ <= n; i__ += 1) {
        nbd[i__ - 1] = 2;
        l[i__ - 1] = 20;
        u[i__ - 1] = 40;
        /* L10: */
    }
    /*     We now define the starting point. */
    for (i__ = 1; i__ <= n; ++i__) {
        x[i__ - 1] = 21;
        /* L14: */
    }
    *task = START;
    /*        ------- the beginning of the loop ---------- */
L111:
    /*     This is the call to the L-BFGS-B code. */
    setulb(&n, &m, x, l, u, nbd, &f, g, &factr, &pgtol, wa, iwa, task, &
            iprint, csave, lsave, isave, dsave);
    if ( IS_FG(*task) ) {
        /*        the minimization routine has returned to request the */
        /*        function f and gradient g values at the current x. */
        /*        Compute function value f for the sample problem. */
        /* Computing 2nd power */
        /*          go back to the minimization routine. */
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

        f = ptr[objective_offset];

        goto L111;
    }

    if (*task==NEW_X ) {

        /*        the minimization routine has returned with a new iterate. */
        /*        At this point have the opportunity of stopping the iteration */
        /*        or observing the values of certain parameters */

        /*        First are two examples of stopping tests. */
        /*        Note: task(1:4) must be assigned the value 'STOP' to terminate */
        /*          the iteration and ensure that the final results are */
        /*          printed in the default format. The rest of the character */
        /*          string TASK may be used to store other information. */
        /* 1) Terminate if the total number of f and g evaluations */
        /*             exceeds 99. */
        if (isave[33] >= 99) {
            *task = STOP_ITER;
/*             s_copy(task, "STOP: TOTAL NO. of f AND g EVALUATIONS EXCEEDS LIM" */
/*                     "IT", (ftnlen)60, (ftnlen)52); */
        }
        /*  2) Terminate if  |proj g|/(1+|f|) < 1.0d-10, where */
        /*           "proj g" denoted the projected gradient */
        if (dsave[12] <= (fabs(f) + 1.) * 1e-10) {
            *task = STOP_GRAD;
/*             s_copy(task, "STOP: THE PROJECTED GRADIENT IS SUFFICIENTLY SMALL", */
/*                     (ftnlen)60, (ftnlen)50); */
        }
        /*        We now wish to print the following information at each */
        /*        iteration: */

        /*          1) the current iteration number, isave(30), */
        /*          2) the total number of f and g evaluations, isave(34), */
        /*          3) the value of the objective function f, */
        /*          4) the norm of the projected gradient,  dsve(13) */

        /*        See the comments at the end of driver1 for a description */
        /*        of the variables isave and dsave. */
        printf("Iterate %5ld  nfg = %4ld   f = %6.4e   |proj g| = %6.4e\n", isave[29], isave[33], f, dsave[12] );
        /*        If the run is to be terminated, we print also the information */
        /*        contained in task as well as the final value of x. */
        if (IS_STOP(*task)) {
            printf(" Final X = \n");
            i__1 = n;
            for (i__ = 1; i__ <= i__1; ++i__) {
                printf("%.3e ", x[i__-1]);
            }
            printf("\n");
        }
        /*          go back to the minimization routine. */
        goto L111;
    }
    /*           ---------- the end of the loop ------------- */
    /*     If task is neither FG nor NEW_X we terminate execution. */
    return 0;
}
