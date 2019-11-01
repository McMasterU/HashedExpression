#ifndef UTILS_C
#define UTILS_C

#include <fftw3.h>
#include <stdio.h>
#include <time.h>
#include <math.h>
#include <stdlib.h>
#include <stdbool.h>
#include <lbfgs.h>
#include <string.h>

typedef enum complex_part {
  REAL = 0, IMAG
} complex_part;

// For simplicity, just use the dft for complex by adding 0 to the input complex part
// We will use the special version for computing dft real later
void dft_1d(int N, double *in, double *out, complex_part part) {
  fftw_complex *aux;
  fftw_plan p;
  int i;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  p = fftw_plan_dft_1d(N, aux, aux, FFTW_FORWARD, FFTW_MEASURE);
  for (i = 0; i < N; i++) {
    aux[i][0] = in[i];
    aux[i][1] = 0;
  }
  fftw_execute(p); /* repeat as needed */


  for (i = 0; i < N; i++) {
    out[i] = aux[i][part];
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}

// For simplicity, just use the dft for complex by adding 0 to the input complex part
// We will use the special version for computing dft real later
void dft_2d(int ROW, int COLUMN, double *in, double *out, complex_part part) {
  fftw_complex *aux;
  fftw_plan p;
  int i, j;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * ROW * COLUMN);
  p = fftw_plan_dft_2d(ROW, COLUMN, aux, aux, FFTW_FORWARD, FFTW_MEASURE);
  for (i = 0; i < ROW; i++) {
    for (j = 0; j < COLUMN; j++) {
      aux[i * COLUMN + j][0] = in[i * COLUMN + j];
      aux[i * COLUMN + j][1] = 0;
    }
  }
  fftw_execute(p); /* repeat as needed */

  for (i = 0; i < ROW; i++) {
    for (j = 0; j < COLUMN; j++) {
      out[i * COLUMN + j] = aux[i * COLUMN + j][part];
    }
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}


// apply (real part . DFT) twice 1d
void re_dft_twice_1d(int N, double *in, double *out) {
  int i;
  for (i = 0; i < N; i++) {
    out[i] = in[i] + in[i == 0 ? 0 : N - i];
    out[i] *= N / 2;
  }
}

// apply (real part . DFT) twice 2d
void re_dft_twice_2d(int ROW, int COLUMN, double *in, double *out) {
  int i, j;
  for (i = 0; i < ROW; i++) {
    for (j = 0; j < COLUMN; j++) {
      out[i * COLUMN + j] = in[i * COLUMN + j] + in[(i == 0 ? 0 : (ROW - i)) * COLUMN + (j == 0 ? 0 : (COLUMN - j))];
      out[i * COLUMN + j] *= ROW * COLUMN / 2;
    }
  }
}

// apply (imag part . DFT) twice 1d. Only take linear time
void im_dft_twice_1d(int N, double *in, double *out) {
  int i;
  for (i = 0; i < N; i++) {
    out[i] = in[i] - in[i == 0 ? 0 : (N - i)];
    out[i] *= N / 2;
  }
}

// apply (imag part . DFT) twice 2d. Only take linear time
void im_dft_twice_2d(int ROW, int COLUMN, double *in, double *out) {
  int i, j;
  for (i = 0; i < ROW; i++) {
    for (j = 0; j < COLUMN; j++) {
      out[i * COLUMN + j] = in[i * COLUMN + j] - in[(i == 0 ? 0 : ROW - i) * COLUMN + (j == 0 ? 0 : COLUMN - j)];
      out[i * COLUMN + j] *= ROW * COLUMN / 2;
    }
  }
}

#endif /* UTILS_C */
