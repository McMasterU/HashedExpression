/**
* ---------------------------FOURIER TRANSFORM HELPERS---------------------------------
* ---------------------------------------START-----------------------------------------
**/
// NOTE THAT IN FFTW, INVERSE FFT IS UNNORMALIZED, WE ARE FOLLOWING THE NORMALIZE STANDARD

#include <fftw3.h>

void dft_1d(int N, double complex *in, double complex *out, int forward_or_backward) {
  fftw_plan p = fftw_plan_dft_1d(N, in, out, forward_or_backward, FFTW_ESTIMATE);
  fftw_execute(p);
  if (forward_or_backward == FFTW_BACKWARD) {
    int i;
    for (i = 0; i < N; i++) {
      out[i] = out[i] / N;
    }
  }

  fftw_destroy_plan(p);
}

void dft_2d(int ROW, int COLUMN, double complex *in, double complex *out, int forward_or_backward) {
  int size = ROW * COLUMN;
  fftw_plan p = fftw_plan_dft_2d(ROW, COLUMN, in, out, forward_or_backward, FFTW_ESTIMATE);
  fftw_execute(p);
  if (forward_or_backward == FFTW_BACKWARD) {
    int i;
    for (i = 0; i < size; i++) {
        out[i] = out[i] / size;
    }
  }

  fftw_destroy_plan(p);
}

/**
* ---------------------------------------END-------------------------------------------
**/
