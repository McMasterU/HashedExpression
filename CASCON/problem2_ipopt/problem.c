#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.c"
#include "hdf5.h"


#define NUM_VARIABLES 1
#define NUM_ACTUAL_VARIABLES 16384
#define MEM_SIZE 393227

// all the actual double variables are allocated
// one after another, starts from here
#define VARS_START_OFFSET 0


const char* var_name[NUM_VARIABLES] = {"x"};
const int var_num_dim[NUM_VARIABLES] = {2};
const int var_shape[NUM_VARIABLES][3] = {{128, 128, 1}};
const int var_size[NUM_VARIABLES] = {16384};
const int var_offset[NUM_VARIABLES] = {0};
const int partial_derivative_offset[NUM_VARIABLES] = {65538};
const int objective_offset = 81922;
double ptr[MEM_SIZE];


const int bound_pos[NUM_VARIABLES] = {0};
double lower_bound[NUM_ACTUAL_VARIABLES];
double upper_bound[NUM_ACTUAL_VARIABLES];


#define NUM_SCALAR_CONSTRAINT 0

double sc_lower_bound[NUM_SCALAR_CONSTRAINT];
double sc_upper_bound[NUM_SCALAR_CONSTRAINT];
const int sc_offset[NUM_SCALAR_CONSTRAINT] = {};

const int sc_partial_derivative_offset[NUM_SCALAR_CONSTRAINT][NUM_VARIABLES] = {};


void read_bounds() {
  for (int i = 0; i < NUM_ACTUAL_VARIABLES; i++) {
    lower_bound[i] = -INFINITY;
    upper_bound[i] = INFINITY;
  }
  {
    printf("Reading x_lb from HDF5 file x_lb.h5 in dataset x_lb....\n");
    hid_t file, dset;
    file = H5Fopen ("x_lb.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, "x_lb", H5P_DEFAULT);
    H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, lower_bound + 0);
    H5Fclose (file);
    H5Dclose (dset);
  }
  {
    printf("Reading x_ub from HDF5 file x_ub.h5 in dataset x_ub....\n");
    hid_t file, dset;
    file = H5Fopen ("x_ub.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, "x_ub", H5P_DEFAULT);
    H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, upper_bound + 0);
    H5Fclose (file);
    H5Dclose (dset);
  }
}
void read_values() {
  srand(time(NULL));
  {
    printf("Reading im from HDF5 file im.h5 in dataset im....\n");
    hid_t file, dset;
    file = H5Fopen ("im.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, "im", H5P_DEFAULT);
    H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptr + 16384);
    H5Fclose (file);
    H5Dclose (dset);
  }
  {
    printf("Reading re from HDF5 file re.h5 in dataset re....\n");
    hid_t file, dset;
    file = H5Fopen ("re.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, "re", H5P_DEFAULT);
    H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptr + 32768);
    H5Fclose (file);
    H5Dclose (dset);
  }
  {
    printf("Reading signal from HDF5 file signal.h5 in dataset signal....\n");
    hid_t file, dset;
    file = H5Fopen ("signal.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, "signal", H5P_DEFAULT);
    H5Dread (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, ptr + 49152);
    H5Fclose (file);
    H5Dclose (dset);
  }
  {
    int i;
    for (i = 0; i < 16384; i++) { 
      *(ptr + 0 + i) = 0.0;
    }
  }
}
void evaluate_partial_derivatives_and_objective() {
  (ptr[65536]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[163843 + i]) = (ptr[32768 + i]) * (ptr[49152 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 311307), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[180228 + i]) = (ptr[49152 + i]) * (ptr[311307 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163843 + i]) * (ptr[180228 + i]);
    }
    (ptr[294917]) = acc;
  }
  (ptr[180227]) = (ptr[65536]) * (ptr[294917]);
  dft_2d(128, 128, (ptr + 0), (ptr + 344075), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[81923 + i]) = (ptr[49152 + i]) * (ptr[344075 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[131075 + i]) = (ptr[16384 + i]) * (ptr[49152 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81923 + i]) * (ptr[131075 + i]);
    }
    (ptr[294920]) = acc;
  }
  (ptr[196612]) = (ptr[65536]) * (ptr[294920]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[180228 + i]) * (ptr[180228 + i]);
    }
    (ptr[294918]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[131075 + i]) * (ptr[131075 + i]);
    }
    (ptr[294919]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81923 + i]) * (ptr[81923 + i]);
    }
    (ptr[294921]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163843 + i]) * (ptr[163843 + i]);
    }
    (ptr[294922]) = acc;
  }
  (ptr[81922]) = (ptr[180227]) + (ptr[196612]) + (ptr[294918]) + (ptr[294919]) + (ptr[294921]) + (ptr[294922]);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[212997 + i]) = pow((ptr[49152 + i]),2);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[114691 + i]) = (ptr[16384 + i]) * (ptr[212997 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 114691), (ptr + 360459), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[229381 + i]) = (ptr[65536])*(ptr[360459 + i]);
    }
  }
  (ptr[65537]) = 2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[196613 + i]) = (ptr[212997 + i]) * (ptr[311307 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 196613), (ptr + 294923), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[245765 + i]) = (ptr[65537])*(ptr[294923 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[147459 + i]) = (ptr[32768 + i]) * (ptr[212997 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 147459), (ptr + 327691), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[262149 + i]) = (ptr[65536])*(ptr[327691 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[98307 + i]) = (ptr[212997 + i]) * (ptr[344075 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 98307), (ptr + 376843), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[278533 + i]) = (ptr[65537])*(ptr[376843 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[65538 + i]) = (ptr[229381 + i]) + (ptr[245765 + i]) + (ptr[262149 + i]) + (ptr[278533 + i]);
    }
  }
}
void evaluate_objective() {
  (ptr[65536]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[163843 + i]) = (ptr[32768 + i]) * (ptr[49152 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 311307), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[180228 + i]) = (ptr[49152 + i]) * (ptr[311307 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163843 + i]) * (ptr[180228 + i]);
    }
    (ptr[294917]) = acc;
  }
  (ptr[180227]) = (ptr[65536]) * (ptr[294917]);
  dft_2d(128, 128, (ptr + 0), (ptr + 344075), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[81923 + i]) = (ptr[49152 + i]) * (ptr[344075 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[131075 + i]) = (ptr[16384 + i]) * (ptr[49152 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81923 + i]) * (ptr[131075 + i]);
    }
    (ptr[294920]) = acc;
  }
  (ptr[196612]) = (ptr[65536]) * (ptr[294920]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[180228 + i]) * (ptr[180228 + i]);
    }
    (ptr[294918]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[131075 + i]) * (ptr[131075 + i]);
    }
    (ptr[294919]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81923 + i]) * (ptr[81923 + i]);
    }
    (ptr[294921]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163843 + i]) * (ptr[163843 + i]);
    }
    (ptr[294922]) = acc;
  }
  (ptr[81922]) = (ptr[180227]) + (ptr[196612]) + (ptr[294918]) + (ptr[294919]) + (ptr[294921]) + (ptr[294922]);
}
void evaluate_partial_derivatives() {
  (ptr[65536]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[212997 + i]) = pow((ptr[49152 + i]),2);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[114691 + i]) = (ptr[16384 + i]) * (ptr[212997 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 114691), (ptr + 360459), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[229381 + i]) = (ptr[65536])*(ptr[360459 + i]);
    }
  }
  (ptr[65537]) = 2.0;
  dft_2d(128, 128, (ptr + 0), (ptr + 311307), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[196613 + i]) = (ptr[212997 + i]) * (ptr[311307 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 196613), (ptr + 294923), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[245765 + i]) = (ptr[65537])*(ptr[294923 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[147459 + i]) = (ptr[32768 + i]) * (ptr[212997 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 147459), (ptr + 327691), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[262149 + i]) = (ptr[65536])*(ptr[327691 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 344075), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[98307 + i]) = (ptr[212997 + i]) * (ptr[344075 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 98307), (ptr + 376843), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[278533 + i]) = (ptr[65537])*(ptr[376843 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[65538 + i]) = (ptr[229381 + i]) + (ptr[245765 + i]) + (ptr[262149 + i]) + (ptr[278533 + i]);
    }
  }
}
void evaluate_scalar_constraints() {
}
void evaluate_scalar_constraints_jacobian() {
}