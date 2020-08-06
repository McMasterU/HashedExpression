/**
* ---------------------------FOURIER TRANSFORM HELPERS---------------------------------
* ---------------------------------------START-----------------------------------------
**/
// NOTE THAT IN FFTW, INVERSE FFT IS UNNORMALIZED, WE ARE FOLLOWING THE NORMALIZE STANDARD

#include <fftw3.h>

// For simplicity, just use the dft for complex by adding 0 to the input complex part
// We will use the special version for computing dft real later
void dft_1d(int N, double *in, double *out, int forward_or_backward) {
  fftw_complex *aux;
  fftw_plan p;
  int i;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  p = fftw_plan_dft_1d(N, aux, aux, forward_or_backward, FFTW_MEASURE);
  for (i = 0; i < N; i++) {
    aux[i][0] = in[i];
    aux[i][1] = in[i + N];
  }
  fftw_execute(p); /* repeat as needed */


  if (forward_or_backward == FFTW_FORWARD) {
    for (i = 0; i < N; i++) {
      out[i] = aux[i][0];
      out[i + N] = aux[i][1];
    }
  } else {
    for (i = 0; i < N; i++) {
      out[i] = aux[i][0] / N;
      out[i + N] = aux[i][1] / N;
    }
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}

// For simplicity, just use the dft for complex by adding 0 to the input complex part
// We will use the special version for computing dft real later
void dft_2d(int ROW, int COLUMN, double *in, double *out, int forward_or_backward) {
  fftw_complex *aux;
  fftw_plan p;
  int i, j;
  int size = ROW * COLUMN;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * size);
  p = fftw_plan_dft_2d(ROW, COLUMN, aux, aux, forward_or_backward, FFTW_MEASURE);
  for (i = 0; i < ROW; i++) {
    for (j = 0; j < COLUMN; j++) {
      aux[i * COLUMN + j][0] = in[i * COLUMN + j];
      aux[i * COLUMN + j][1] = in[i * COLUMN + j + size];
    }
  }
  fftw_execute(p); /* repeat as needed */

  if (forward_or_backward == FFTW_FORWARD) {
    for (i = 0; i < ROW; i++) {
      for (j = 0; j < COLUMN; j++) {
        out[i * COLUMN + j] = aux[i * COLUMN + j][0];
        out[i * COLUMN + j + size] = aux[i * COLUMN + j][1];
      }
    }
  } else {
    for (i = 0; i < ROW; i++) {
      for (j = 0; j < COLUMN; j++) {
        out[i * COLUMN + j] = aux[i * COLUMN + j][0] / size;
        out[i * COLUMN + j + size] = aux[i * COLUMN + j][1] / size;
      }
    }
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}

/**
* ---------------------------------------END-------------------------------------------
**/
