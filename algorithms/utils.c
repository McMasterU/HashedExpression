#include <fftw3.h>

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

// For simplicity, just use the dft for complex by adding 0 to the input complex part
// We will use the special version for computing dft real later
void dft_2d(int M, int N, double *in, double *out, complex_part part) {
  fftw_complex *aux;
  fftw_plan p;
  int i, j;
  aux = (fftw_complex*) fftw_malloc(sizeof(fftw_complex) * M * N);
  for (i = 0; i < M; i++) {
    for (j = 0; j < N; j++) {
      aux[i * N + j][0] = in[i * N + j];
      aux[i * N + j][1] = 0;
    }
  }
  p = fftw_plan_dft_2d(M, N, aux, aux, FFTW_FORWARD, FFTW_MEASURE);
  fftw_execute(p); /* repeat as needed */

  for (i = 0; i < M; i++) {
    for (j = 0; j < N; j++) {
      out[i * N + j] = aux[i * N + j][part];
    }
  }

  fftw_destroy_plan(p);
  fftw_free(aux);
}
