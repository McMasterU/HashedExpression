#include <math.h>
#include <stdio.h>
#include <stdlib.h>


const int NUM_VARIABLES = 1;
const int MEM_SIZE = 713;
const int var_size[NUM_VARIABLES] = {100};
const int var_offset[NUM_VARIABLES] = {100};
const int partial_derivative_offset[NUM_VARIABLES] = {301};
const int objective_offset = 706;
double ptr[MEM_SIZE];


void assign_values() {
  (ptr[0]) = 1.0;
  (ptr[0 + 1]) = 2.0;
  (ptr[0 + 2]) = 3.0;
  (ptr[0 + 3]) = 4.0;
  (ptr[0 + 4]) = 5.0;
  (ptr[0 + 5]) = 6.0;
  (ptr[0 + 6]) = 7.0;
  (ptr[0 + 7]) = 8.0;
  (ptr[0 + 8]) = 9.0;
  (ptr[0 + 9]) = 10.0;
  (ptr[0 + 10]) = 11.0;
  (ptr[0 + 11]) = 12.0;
  (ptr[0 + 12]) = 13.0;
  (ptr[0 + 13]) = 14.0;
  (ptr[0 + 14]) = 15.0;
  (ptr[0 + 15]) = 16.0;
  (ptr[0 + 16]) = 17.0;
  (ptr[0 + 17]) = 18.0;
  (ptr[0 + 18]) = 19.0;
  (ptr[0 + 19]) = 20.0;
  (ptr[0 + 20]) = 21.0;
  (ptr[0 + 21]) = 22.0;
  (ptr[0 + 22]) = 23.0;
  (ptr[0 + 23]) = 24.0;
  (ptr[0 + 24]) = 25.0;
  (ptr[0 + 25]) = 26.0;
  (ptr[0 + 26]) = 27.0;
  (ptr[0 + 27]) = 28.0;
  (ptr[0 + 28]) = 29.0;
  (ptr[0 + 29]) = 30.0;
  (ptr[0 + 30]) = 31.0;
  (ptr[0 + 31]) = 32.0;
  (ptr[0 + 32]) = 33.0;
  (ptr[0 + 33]) = 34.0;
  (ptr[0 + 34]) = 35.0;
  (ptr[0 + 35]) = 36.0;
  (ptr[0 + 36]) = 37.0;
  (ptr[0 + 37]) = 38.0;
  (ptr[0 + 38]) = 39.0;
  (ptr[0 + 39]) = 40.0;
  (ptr[0 + 40]) = 41.0;
  (ptr[0 + 41]) = 42.0;
  (ptr[0 + 42]) = 43.0;
  (ptr[0 + 43]) = 44.0;
  (ptr[0 + 44]) = 45.0;
  (ptr[0 + 45]) = 46.0;
  (ptr[0 + 46]) = 47.0;
  (ptr[0 + 47]) = 48.0;
  (ptr[0 + 48]) = 49.0;
  (ptr[0 + 49]) = 50.0;
  (ptr[0 + 50]) = 51.0;
  (ptr[0 + 51]) = 52.0;
  (ptr[0 + 52]) = 53.0;
  (ptr[0 + 53]) = 54.0;
  (ptr[0 + 54]) = 55.0;
  (ptr[0 + 55]) = 56.0;
  (ptr[0 + 56]) = 57.0;
  (ptr[0 + 57]) = 58.0;
  (ptr[0 + 58]) = 59.0;
  (ptr[0 + 59]) = 60.0;
  (ptr[0 + 60]) = 61.0;
  (ptr[0 + 61]) = 62.0;
  (ptr[0 + 62]) = 63.0;
  (ptr[0 + 63]) = 64.0;
  (ptr[0 + 64]) = 65.0;
  (ptr[0 + 65]) = 66.0;
  (ptr[0 + 66]) = 67.0;
  (ptr[0 + 67]) = 68.0;
  (ptr[0 + 68]) = 69.0;
  (ptr[0 + 69]) = 70.0;
  (ptr[0 + 70]) = 71.0;
  (ptr[0 + 71]) = 72.0;
  (ptr[0 + 72]) = 73.0;
  (ptr[0 + 73]) = 74.0;
  (ptr[0 + 74]) = 75.0;
  (ptr[0 + 75]) = 76.0;
  (ptr[0 + 76]) = 77.0;
  (ptr[0 + 77]) = 78.0;
  (ptr[0 + 78]) = 79.0;
  (ptr[0 + 79]) = 80.0;
  (ptr[0 + 80]) = 81.0;
  (ptr[0 + 81]) = 82.0;
  (ptr[0 + 82]) = 83.0;
  (ptr[0 + 83]) = 84.0;
  (ptr[0 + 84]) = 85.0;
  (ptr[0 + 85]) = 86.0;
  (ptr[0 + 86]) = 87.0;
  (ptr[0 + 87]) = 88.0;
  (ptr[0 + 88]) = 89.0;
  (ptr[0 + 89]) = 90.0;
  (ptr[0 + 90]) = 91.0;
  (ptr[0 + 91]) = 92.0;
  (ptr[0 + 92]) = 93.0;
  (ptr[0 + 93]) = 94.0;
  (ptr[0 + 94]) = 95.0;
  (ptr[0 + 95]) = 96.0;
  (ptr[0 + 96]) = 97.0;
  (ptr[0 + 97]) = 98.0;
  (ptr[0 + 98]) = 99.0;
  (ptr[0 + 99]) = 100.0;
}


void evaluate_partial_derivatives_and_objective() {
  {
    int i;
    for (i = 0; i < 100; i++) {
      (ptr[406 + i]) = -(ptr[0 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 100; i++) {
      (ptr[201 + i]) = (ptr[100 + i]) + (ptr[406 + i]);
    }
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
      acc += (ptr[201 + i]) * (ptr[201 + i]);
    }
    (ptr[709]) = acc;
  }
  (ptr[706]) = sqrt(ptr[709]);
  (ptr[200]) = -1.0;
  {
    double acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
      acc += (ptr[0 + i]) * (ptr[100 + i]);
    }
    (ptr[710]) = acc;
  }
  (ptr[402]) = (ptr[200]) * (ptr[710]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
      acc += (ptr[100 + i]) * (ptr[0 + i]);
    }
    (ptr[708]) = acc;
  }
  (ptr[404]) = (ptr[200]) * (ptr[708]);
  {
    double acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
      acc += (ptr[100 + i]) * (ptr[100 + i]);
    }
    (ptr[711]) = acc;
  }
  {
    double acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
      acc += (ptr[0 + i]) * (ptr[0 + i]);
    }
    (ptr[712]) = acc;
  }
  (ptr[401]) = (ptr[402]) + (ptr[404]) + (ptr[711]) + (ptr[712]);
  (ptr[707]) = sqrt(ptr[401]);
  (ptr[405]) = pow((ptr[707]),-1);
  {
    int i;
    for (i = 0; i < 100; i++) {
      (ptr[506 + i]) = (ptr[405])*(ptr[100 + i]);
    }
  }
  (ptr[403]) = (ptr[200]) * (ptr[405]);
  {
    int i;
    for (i = 0; i < 100; i++) {
      (ptr[606 + i]) = (ptr[403])*(ptr[0 + i]);
    }
  }
  {
    int i;
    for (i = 0; i < 100; i++) {
      (ptr[301 + i]) = (ptr[506 + i]) + (ptr[606 + i]);
    }
  }
}