#include "problem.c"
#include <math.h>
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#define oo 10000000

extern const char* var_name[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_num_dim[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_shape[NUM_HIGH_DIMENSIONAL_VARIABLES][3];
extern const int var_size[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int var_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int partial_derivative_offset[NUM_HIGH_DIMENSIONAL_VARIABLES];
extern const int objective_offset;
extern double ptr[MEM_SIZE];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();


const int MAX_ITER = 50000;
const double PRECISION = 1e-6;

double random_in(double min, double max) {
  double range = (max - min);
  double div = RAND_MAX / range;
  return min + (rand() / div);
}


bool any_partial_derivative_NaN() {
  int i, j;
  for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      if (isnan(ptr[partial_derivative_offset[i] + j])) {
        return true;
      }
    }
  }

  return false;
}

int main() {
  srand(time(NULL));
  int i, j, cnt;
  int total_variable_size = 0;
  for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
    total_variable_size = total_variable_size + var_size[i];
  }

  double *var_temp = malloc(sizeof(double) * total_variable_size);
  double *grad_temp = malloc(sizeof(double) * total_variable_size);

  // assign values to predefined variables
  assign_values();

  while (true) {
    // initialize optimizing variables
    for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
      for (j = 0; j < var_size[i]; j++) {
        ptr[var_offset[i] + j] = random_in(0, 1);
      }
    }
    evaluate_partial_derivatives_and_objective();
    if (any_partial_derivative_NaN()) continue;


    int iter = 0;
    while (iter < MAX_ITER) {
      // save the state of all variables
      cnt = 0;
      for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          var_temp[cnt] = ptr[var_offset[i] + j];
          grad_temp[cnt] = ptr[partial_derivative_offset[i] + j];
          cnt++;
        }
      }

      // fx
      double fx = ptr[objective_offset];
      double minus_dot_grad = 0;
      for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          minus_dot_grad = minus_dot_grad - (ptr[partial_derivative_offset[i] + j] * ptr[partial_derivative_offset[i] + j]);
        }
      }

      double beta = 0.8;
      double t = 1;

      // after while, x still the same
      while (t > 0) {
        cnt = 0;
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] -= t * grad_temp[cnt];
            cnt++;
          }
        }
        evaluate_partial_derivatives_and_objective();


        cnt = 0;
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] = var_temp[cnt];
            cnt++;
          }
        }

        if (ptr[objective_offset] > fx + 0.5 * t * minus_dot_grad) {

          t = beta * t;
          continue;

        } else {
          break;
        }
      }


      while (t > 0 && any_partial_derivative_NaN()) {
        t = t / 2;
        cnt = 0;
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            var_temp[cnt] = ptr[var_offset[i] + j];
            cnt++;
          }
        }

        cnt = 0;
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] -= t * grad_temp[cnt];
            cnt++;
          }
        }
        evaluate_partial_derivatives_and_objective();
        cnt = 0;
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] = var_temp[cnt];
            cnt++;
          }
        }
      }

      if (t == 0) {
        printf("Couldn't move further, stop gradient descent here. \n");
        break;
      }


      double max_step = 0;
      // OK now we have a good t, let's update
      cnt = 0;
      for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          double step = -t * grad_temp[cnt];
          ptr[var_offset[i] + j] += step;
          max_step = fmax(max_step, fabs(step));
          cnt++;
        }
      }
      evaluate_partial_derivatives_and_objective();
      iter++;
      if (iter / 1000 > (iter - 1) / 1000) {
        printf("iter = %d\n", iter);
        printf("ptr[objective_offset] = %f\n", ptr[objective_offset]);
      }


      if (max_step < PRECISION) {
        printf("t = %f\n", t);
        printf("max_step = %f\n", max_step);
        break;
      }

    }

    if (!isnan(ptr[objective_offset])) {
      printf("f_min = %f\n", ptr[objective_offset]);
      printf("Writing result to output.txt...\n");
      FILE *fp = fopen("output.txt", "w");
      if (fp) {
        for (i = 0; i < NUM_HIGH_DIMENSIONAL_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            fprintf(fp, "%f ", ptr[var_offset[i] + j]);
          }
          fprintf(fp, "\n");
        }
      }
      printf("Done\n");
      break;
    }
  }

  free(var_temp);
  free(grad_temp);
}
