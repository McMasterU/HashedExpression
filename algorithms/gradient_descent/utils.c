#include <fftw3.h>

typedef enum complex_part {
  REAL = 0, IMAG
} complex_part;

void dft_1d(int N, double *in, double *out, complex_part part) {
  fftw_complex *aux;
  fftw_plan p;
  int i;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * N);
  for (i = 0; i < N; i++) {
    aux[i][0] = in[i];
    aux[i][1] = 0;
  }
  p = fftw_plan_dft_1d(N, aux, aux, FFTW_FORWARD, FFTW_MEASURE);
  fftw_execute(p); /* repeat as needed */

  for (i = 0; i < N; i++) {
    out[i] = aux[i][part];
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}

