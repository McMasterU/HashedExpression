## HashedExpression [![wercker status](https://app.wercker.com/status/fce29884fa47e4258f62240000f1e368/s/master "wercker status")](https://app.wercker.com/project/byKey/fce29884fa47e4258f62240000f1e368)

A type-safe symbolic computing Haskell embeded DSL for solving optimization problems.

## Symphony

Symphony is a standalone language backed by HashedExpression, it somewhat resembles AMPL (but free).
### Installation
We haven't yet publish it to Cabal (or Homebrew, apt-get), in the mean time, you can clone this repository and run:
```terminal
$ stack install --ghc-options -O2
```

### Usage
The following example is minizing the simple entropy function [![image](https://latex.codecogs.com/gif.latex?%5Cdpi%7B200%7D%20%5CLARGE%20f%28p%29%20%3D%20%5Csum_%7Bi%20%3D%201%7D%5En%20p_i%20%5Clog%28p_i%29)]

Create `rosenbrock.sp`: 
```haskell
variables:
  x[10][10]

constraints: 
  x > 1

```



Below is an example of an optimization problem to reconstruct image from loss MRI signal, details about the problem can be found at
[MRI-Image-Reconstruction](examples/MRI-Image-Reconstruction.pdf).

```haskell
variables:
  x[128][128] = 0

constants:
  im[128][128] = Dataset("data.h5", "im")
  re[128][128] = Dataset("data.h5", "re")
  signal[128][128] = Dataset("data.h5", "signal")
  xLowerBound[128][128] = Dataset("data.h5", "x_lb")
  xUpperBound[128][128] = Dataset("data.h5", "x_ub")

constraints:
  x >= xLowerBound, x <= xUpperBound

let:
  smootherX = rotate (0, 1) x + rotate (0, -1) x - 2 *. x
  smootherY = rotate (1, 0) x + rotate (-1, 0) x - 2 *. x
  regularization = huberNorm 2 smootherX + huberNorm 2 smootherY

minimize:
  norm2square ((signal +: 0) * (ft x - (re +: im))) + 3000 *. regularization
```
Running:

```terminal
$ symphony mri.sp
```

### Usage Docker
To use symphony through docker (if you wish to avoid installing stack), build the docker image with
```terminal
$ docker build . -t symphony
```
Then run the docker container. In order to provide the input file to the container, you'll have to link 
the path (say */some/path/*) containing the the symphony file on your host machine to the */target* path
in the container
```terminal
docker run -v /some/path:/target symphony
```
Copy the resulting */some/path/problem.c* into your chosen *algorithms* directory (i.e ipopt, lbfgs, etc),
which contain their own Dockerfile's for executing the optimization problem (see respective README's)

## Contributing
Please read `Contributing.md`. PRs are welcome.


## About
The project is developed and maintained by [Dr. Christopher Anand](https://github.com/christopheranand)'s research group, Computing and Software department, McMaster University.

List of contributors:
- [Nhan Thai](https://github.com/dandoh)
- [Curtis D'alves](https://github.com/dalvescb)
- [Christopher Anand](https://github.com/christopheranand)
- ...

