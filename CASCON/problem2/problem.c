#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "utils.c"
#include "hdf5.h"


#define NUM_VARIABLES 1
#define NUM_ACTUAL_VARIABLES 16384
#define MEM_SIZE 671774

// all the actual double variables are allocated
// one after another, starts from here
#define VARS_START_OFFSET 0


const char* var_name[NUM_VARIABLES] = {"x"};
const int var_num_dim[NUM_VARIABLES] = {2};
const int var_shape[NUM_VARIABLES][3] = {{128, 128, 1}};
const int var_size[NUM_VARIABLES] = {16384};
const int var_offset[NUM_VARIABLES] = {0};
const int partial_derivative_offset[NUM_VARIABLES] = {65543};
const int objective_offset = 81927;
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
  (ptr[65540]) = 6000.0;
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 1 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[524318 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 127 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[540702 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[524318 + i]) * (ptr[540702 + i]);
    }
    (ptr[442391]) = acc;
  }
  (ptr[114696]) = (ptr[65540]) * (ptr[442391]);
  (ptr[65536]) = -12000.0;
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[524318 + i]);
    }
    (ptr[442388]) = acc;
  }
  (ptr[114697]) = (ptr[65536]) * (ptr[442388]);
  (ptr[65541]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[163850 + i]) = (ptr[32768 + i]) * (ptr[49152 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 589854), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[180240 + i]) = (ptr[49152 + i]) * (ptr[589854 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163850 + i]) * (ptr[180240 + i]);
    }
    (ptr[442385]) = acc;
  }
  (ptr[180234]) = (ptr[65541]) * (ptr[442385]);
  (ptr[65539]) = 36000.0;
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[0 + i]);
    }
    (ptr[442389]) = acc;
  }
  (ptr[180235]) = (ptr[65539]) * (ptr[442389]);
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 1 + 128 ) % 128;
          (ptr[557086 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[557086 + i]);
    }
    (ptr[442392]) = acc;
  }
  (ptr[180236]) = (ptr[65536]) * (ptr[442392]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[540702 + i]);
    }
    (ptr[442386]) = acc;
  }
  (ptr[180237]) = (ptr[65536]) * (ptr[442386]);
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 127 + 128 ) % 128;
          (ptr[475166 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[475166 + i]) * (ptr[557086 + i]);
    }
    (ptr[442396]) = acc;
  }
  (ptr[180238]) = (ptr[65540]) * (ptr[442396]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[475166 + i]);
    }
    (ptr[442397]) = acc;
  }
  (ptr[180239]) = (ptr[65536]) * (ptr[442397]);
  dft_2d(128, 128, (ptr + 0), (ptr + 622622), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[81928 + i]) = (ptr[49152 + i]) * (ptr[622622 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[131082 + i]) = (ptr[16384 + i]) * (ptr[49152 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81928 + i]) * (ptr[131082 + i]);
    }
    (ptr[442393]) = acc;
  }
  (ptr[196624]) = (ptr[65541]) * (ptr[442393]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[180240 + i]) * (ptr[180240 + i]);
    }
    (ptr[442387]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[131082 + i]) * (ptr[131082 + i]);
    }
    (ptr[442390]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81928 + i]) * (ptr[81928 + i]);
    }
    (ptr[442394]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163850 + i]) * (ptr[163850 + i]);
    }
    (ptr[442395]) = acc;
  }
  (ptr[81927]) = (ptr[114696]) + (ptr[114697]) + (ptr[180234]) + (ptr[180235]) + (ptr[180236]) + (ptr[180237]) + (ptr[180238]) + (ptr[180239]) + (ptr[196624]) + (ptr[442387]) + (ptr[442390]) + (ptr[442394]) + (ptr[442395]);
  (ptr[65537]) = -24000.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[229393 + i]) = (ptr[65537])*(ptr[475166 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[245777 + i]) = (ptr[65537])*(ptr[557086 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[213009 + i]) = pow((ptr[49152 + i]),2);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[114698 + i]) = (ptr[16384 + i]) * (ptr[213009 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 114698), (ptr + 639006), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[262161 + i]) = (ptr[65541])*(ptr[639006 + i]);
    }
  }
  (ptr[65538]) = 72000.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[278545 + i]) = (ptr[65538])*(ptr[0 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 2 + 128 ) % 128;
          (ptr[442398 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[294929 + i]) = (ptr[65540])*(ptr[442398 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[311313 + i]) = (ptr[65537])*(ptr[524318 + i]);
    }
  }
  (ptr[65542]) = 2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[196625 + i]) = (ptr[213009 + i]) * (ptr[589854 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 196625), (ptr + 573470), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[327697 + i]) = (ptr[65542])*(ptr[573470 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[147466 + i]) = (ptr[32768 + i]) * (ptr[213009 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 147466), (ptr + 606238), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[344081 + i]) = (ptr[65541])*(ptr[606238 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[98312 + i]) = (ptr[213009 + i]) * (ptr[622622 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 98312), (ptr + 655390), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[360465 + i]) = (ptr[65542])*(ptr[655390 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[376849 + i]) = (ptr[65537])*(ptr[540702 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 2 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[458782 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[393233 + i]) = (ptr[65540])*(ptr[458782 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 126 + 128 ) % 128;
          (ptr[491550 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[409617 + i]) = (ptr[65540])*(ptr[491550 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 126 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[507934 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[426001 + i]) = (ptr[65540])*(ptr[507934 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[65543 + i]) = (ptr[229393 + i]) + (ptr[245777 + i]) + (ptr[262161 + i]) + (ptr[278545 + i]) + (ptr[294929 + i]) + (ptr[311313 + i]) + (ptr[327697 + i]) + (ptr[344081 + i]) + (ptr[360465 + i]) + (ptr[376849 + i]) + (ptr[393233 + i]) + (ptr[409617 + i]) + (ptr[426001 + i]);
    }
  }
}
void evaluate_objective() {
  (ptr[65540]) = 6000.0;
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 1 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[524318 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 127 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[540702 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[524318 + i]) * (ptr[540702 + i]);
    }
    (ptr[442391]) = acc;
  }
  (ptr[114696]) = (ptr[65540]) * (ptr[442391]);
  (ptr[65536]) = -12000.0;
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[524318 + i]);
    }
    (ptr[442388]) = acc;
  }
  (ptr[114697]) = (ptr[65536]) * (ptr[442388]);
  (ptr[65541]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[163850 + i]) = (ptr[32768 + i]) * (ptr[49152 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 589854), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[180240 + i]) = (ptr[49152 + i]) * (ptr[589854 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163850 + i]) * (ptr[180240 + i]);
    }
    (ptr[442385]) = acc;
  }
  (ptr[180234]) = (ptr[65541]) * (ptr[442385]);
  (ptr[65539]) = 36000.0;
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[0 + i]);
    }
    (ptr[442389]) = acc;
  }
  (ptr[180235]) = (ptr[65539]) * (ptr[442389]);
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 1 + 128 ) % 128;
          (ptr[557086 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[557086 + i]);
    }
    (ptr[442392]) = acc;
  }
  (ptr[180236]) = (ptr[65536]) * (ptr[442392]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[540702 + i]);
    }
    (ptr[442386]) = acc;
  }
  (ptr[180237]) = (ptr[65536]) * (ptr[442386]);
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 127 + 128 ) % 128;
          (ptr[475166 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[475166 + i]) * (ptr[557086 + i]);
    }
    (ptr[442396]) = acc;
  }
  (ptr[180238]) = (ptr[65540]) * (ptr[442396]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[0 + i]) * (ptr[475166 + i]);
    }
    (ptr[442397]) = acc;
  }
  (ptr[180239]) = (ptr[65536]) * (ptr[442397]);
  dft_2d(128, 128, (ptr + 0), (ptr + 622622), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[81928 + i]) = (ptr[49152 + i]) * (ptr[622622 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[131082 + i]) = (ptr[16384 + i]) * (ptr[49152 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81928 + i]) * (ptr[131082 + i]);
    }
    (ptr[442393]) = acc;
  }
  (ptr[196624]) = (ptr[65541]) * (ptr[442393]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[180240 + i]) * (ptr[180240 + i]);
    }
    (ptr[442387]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[131082 + i]) * (ptr[131082 + i]);
    }
    (ptr[442390]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[81928 + i]) * (ptr[81928 + i]);
    }
    (ptr[442394]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 16384; i++) {
      acc += (ptr[163850 + i]) * (ptr[163850 + i]);
    }
    (ptr[442395]) = acc;
  }
  (ptr[81927]) = (ptr[114696]) + (ptr[114697]) + (ptr[180234]) + (ptr[180235]) + (ptr[180236]) + (ptr[180237]) + (ptr[180238]) + (ptr[180239]) + (ptr[196624]) + (ptr[442387]) + (ptr[442390]) + (ptr[442394]) + (ptr[442395]);
}
void evaluate_partial_derivatives() {
  (ptr[65537]) = -24000.0;
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 127 + 128 ) % 128;
          (ptr[475166 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[229393 + i]) = (ptr[65537])*(ptr[475166 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 1 + 128 ) % 128;
          (ptr[557086 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[245777 + i]) = (ptr[65537])*(ptr[557086 + i]);
    }
  }
  (ptr[65541]) = -2.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[213009 + i]) = pow((ptr[49152 + i]),2);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[114698 + i]) = (ptr[16384 + i]) * (ptr[213009 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 114698), (ptr + 639006), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[262161 + i]) = (ptr[65541])*(ptr[639006 + i]);
    }
  }
  (ptr[65538]) = 72000.0;
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[278545 + i]) = (ptr[65538])*(ptr[0 + i]);
    }
  }
  (ptr[65540]) = 6000.0;
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 2 + 128 ) % 128;
          (ptr[442398 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[294929 + i]) = (ptr[65540])*(ptr[442398 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 1 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[524318 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[311313 + i]) = (ptr[65537])*(ptr[524318 + i]);
    }
  }
  (ptr[65542]) = 2.0;
  dft_2d(128, 128, (ptr + 0), (ptr + 589854), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[196625 + i]) = (ptr[213009 + i]) * (ptr[589854 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 196625), (ptr + 573470), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[327697 + i]) = (ptr[65542])*(ptr[573470 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[147466 + i]) = (ptr[32768 + i]) * (ptr[213009 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 147466), (ptr + 606238), REAL);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[344081 + i]) = (ptr[65541])*(ptr[606238 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 0), (ptr + 622622), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[98312 + i]) = (ptr[213009 + i]) * (ptr[622622 + i]);
    }
  }
  dft_2d(128, 128, (ptr + 98312), (ptr + 655390), IMAG);
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[360465 + i]) = (ptr[65542])*(ptr[655390 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 127 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[540702 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[376849 + i]) = (ptr[65537])*(ptr[540702 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 2 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[458782 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[393233 + i]) = (ptr[65540])*(ptr[458782 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 0 + 128 ) % 128;
          int aj = (j - 126 + 128 ) % 128;
          (ptr[491550 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[409617 + i]) = (ptr[65540])*(ptr[491550 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 128; i++) {
      {
        int j;
        for (j = 0; j < 128; j++) {
          int ai = (i - 126 + 128 ) % 128;
          int aj = (j - 0 + 128 ) % 128;
          (ptr[507934 + i * 128 + j]) = (ptr[0 + ai * 128 + aj]);
        }
      }
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[426001 + i]) = (ptr[65540])*(ptr[507934 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 16384; i++) {
      (ptr[65543 + i]) = (ptr[229393 + i]) + (ptr[245777 + i]) + (ptr[262161 + i]) + (ptr[278545 + i]) + (ptr[294929 + i]) + (ptr[311313 + i]) + (ptr[327697 + i]) + (ptr[344081 + i]) + (ptr[360465 + i]) + (ptr[376849 + i]) + (ptr[393233 + i]) + (ptr[409617 + i]) + (ptr[426001 + i]);
    }
  }
}
void evaluate_scalar_constraints() {
}
void evaluate_scalar_constraints_jacobian() {
}