#include "problem.c"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#define oo 10000000

extern const int var_size[NUM_VARIABLES];
extern const int var_offset[NUM_VARIABLES];
extern const int partial_derivative_offset[NUM_VARIABLES];
extern const int objective_offset;
extern double ptr[MEM_SIZE];

extern void assign_values();
extern void evaluate_partial_derivatives_and_objective();


const double c1 = 1e-4;
const double c2 = 0.9;
const int MAX_ITER = 4000;
const double PRECISION = 1e-6;

double random_in(double min, double max) {
  double range = (max - min);
  double div = RAND_MAX / range;
  return min + (rand() / div);
}

bool any_partial_derivative_NaN() {
  int i, j;
  for (i = 0; i < NUM_VARIABLES; i++) {
    for (j = 0; j < var_size[i]; j++) {
      if (isnan(ptr[partial_derivative_offset[i] + j])) {
        return true;
      }
    }
  }

  return false;
}

int main() {
  int i, j, k, cnt;
  int total_variable_size = 0;
  for (i = 0; i < NUM_VARIABLES; i++) {
    total_variable_size = total_variable_size + var_size[i];
  }

  double *var_temp = malloc(sizeof(double) * total_variable_size);
  double *grad_temp = malloc(sizeof(double) * total_variable_size);

  // assign values to predefined variables
  assign_values();

  while (true) {
    // initialize optimizing variables
    for (i = 0; i < NUM_VARIABLES; i++) {
      for (j = 0; j < var_size[i]; j++) {
        ptr[var_offset[i] + j] = random_in(0, 1);
      }
    }
    evaluate_partial_derivatives_and_objective();
    if (any_partial_derivative_NaN()) continue;

    double begin_t = random_in(0.9, 1);

    int iter = 0;
    while (iter < MAX_ITER) {
      bool find_t = true;

      // save the state of all variables
      cnt = 0;
      for (i = 0; i < NUM_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          var_temp[cnt] = ptr[var_offset[i] + j];
          grad_temp[cnt] = ptr[partial_derivative_offset[i] + j];
          cnt++;
        }
      }

      // fx
      double fx = ptr[objective_offset];
      double minus_dot_grad = 0;
      for (i = 0; i < NUM_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          minus_dot_grad = minus_dot_grad - (ptr[partial_derivative_offset[i] + j] * ptr[partial_derivative_offset[i] + j]);
        }
      }

      // find the step t that satisfy 2 wolf conditions
      double alpha = 0; // lower bound
      double beta = oo;
      double t = begin_t;

      // after while, x still the same
      while (true) {
        // write updated variables
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] -= t * ptr[partial_derivative_offset[i] + j];
          }
        }
        evaluate_partial_derivatives_and_objective();
        double fnew = ptr[objective_offset];

        cnt = 0;
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] = var_temp[cnt];
            cnt++;
          }
        }
        if (fnew > fx + c1 * t * minus_dot_grad) {
          beta = t;
          t = (alpha + beta) / 2;
        } else {
          // acc = - grad(old) <.> grad(new)
          double acc = 0;
          cnt = 0;
          for (i = 0; i < NUM_VARIABLES; i++) {
            for (j = 0; j < var_size[i]; j++) {
              acc = acc - grad_temp[cnt] * ptr[partial_derivative_offset[i] + j];
              cnt++;
            }
          }
          if (acc < c2 * minus_dot_grad) {
            alpha = t;
            if (beta == oo) {
              t = 2 * alpha;
            } else {
              t = (alpha + beta) / 2;
            }
          } else {
            break;
          }
        }



        if (fabs(alpha - beta) < 1e-7) {
          find_t = false;
          break;
        }
      }


      while (t > 0 && any_partial_derivative_NaN()) {
        t = t / 2;
        cnt = 0;
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            var_temp[cnt] = ptr[var_offset[i] + j];
            cnt++;
          }
        }

        cnt = 0;
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] -= t * grad_temp[cnt];
            cnt++;
          }
        }
        evaluate_partial_derivatives_and_objective();
        cnt = 0;
        for (i = 0; i < NUM_VARIABLES; i++) {
          for (j = 0; j < var_size[i]; j++) {
            ptr[var_offset[i] + j] = var_temp[cnt];
            cnt++;
          }
        }
      }

      if (!find_t && t == 0) {
        printf("Couldn't move further, stop gradient descent here. \n");
        break;
      }


      double max_step = 0;
      // OK now we have a good t, let's update
      cnt = 0;
      for (i = 0; i < NUM_VARIABLES; i++) {
        for (j = 0; j < var_size[i]; j++) {
          double step = -t * grad_temp[cnt];
          ptr[var_offset[i] + j] += step;
          max_step = fmax(max_step, fabs(step));
          cnt++;
        }
      }
      evaluate_partial_derivatives_and_objective();
      iter++;
      if (max_step < PRECISION) break;

    }

    if (!isnan(ptr[objective_offset])) {
      printf("After %d iteration\n", iter);
      printf("f_min = %f at:\n", ptr[objective_offset]);
      for (i = 0; i < NUM_VARIABLES; i++) {
        printf("var[%d] = [", i);
        for (j = 0; j < var_size[i]; j++) {
          printf("%f", ptr[var_offset[i] + j]);
          printf(j == var_size[i] - 1 ? "]\n" : ", ");
        }
      }
      break;
    }
  }

  free(var_temp);
  free(grad_temp);
}