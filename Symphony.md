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
Consider minimizing the negative entropy function ![image](https://latex.codecogs.com/gif.latex?%5Cdpi%7B100%7D%20%5Chuge%20f%28p%29%20%3D%20%5Csum_%7Bi%20%3D%201%7D%5En%20p_i%20%5Clog%28p_i%29)

Create `entropy.sp`: 
```haskell
variables:
  p[10][10]

constraints: 
  p >= 0.1

minimize:
  p <.> log (p) 

solver: lbfgs-b
```
(<.> is dot product)

Run symphony:
```terminal
$ symphony entropy.sp
```

Symphony will generate code and download the solver (L-BFGS-B in this case) to current working directory. 
Then simply compile the code and run:
```terminal
$ make
```
(Note that we use [HDF5](https://www.hdfgroup.org/solutions/hdf5/) for reading and writing dataset)

```terminal
$ ./lbfgs-b
....
....
F     = final function value
           * * * 
   N    Tit   Tnf  Tnint  Skip  Nact      Projg        F
  100     7     8    45     0     0     4.36e-06 -3.67879e+01
21
 Total User time 2.470e-04 seconds.
Writing p to p_out.h5...     
```
Which is what we expected, and the output value of `p` is written in HDF5 file `p_out.h5`.


For more, checkout `examples/Brain/brain.sp` example which solves an optimization problem to reconstruct image from loss MRI signal, details about the problem can be found at [MRI-Image-Reconstruction](docs/images/MRI-Image-Reconstruction.pdf).
```haskell
variables:
  x[128][128] = 0

constants:
  im[128][128] = Dataset("kspace.h5", "im")
  re[128][128] = Dataset("kspace.h5", "re")
  mask[128][128] = Dataset("mask.h5", "mask")
  xLowerBound[128][128] = Dataset("bound.h5", "lb")
  xUpperBound[128][128] = Dataset("bound.h5", "ub")

constraints:
  x >= xLowerBound, x <= xUpperBound

let:
  smootherX = rotate (0, 1) x + rotate (0, -1) x - 2 *. x
  smootherY = rotate (1, 0) x + rotate (-1, 0) x - 2 *. x
  regularization = norm2square smootherX + norm2square smootherY


minimize:
  norm2square ((mask +: 0) * (ft x - (re +: im))) + 3000 *. regularization
  
solver: lbfgs-b
```


### Docker 
We provide ready-to-use and up-to-date docker image [hashexpression/symphony](https://hub.docker.com/r/hashexpression/symphony) which has 
`symphony` and all prerequisites installed. All you need is pull and create a container.