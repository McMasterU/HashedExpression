# HashedExpression & Symphony   [![CircleCI](https://circleci.com/gh/dalvescb/HashedExpression/tree/master.svg?style=svg)](https://circleci.com/gh/dalvescb/HashedExpression/tree/master)
Tensor-aware symbolic computing, code generation for optimization problems, and more.

## Overview
This project contains:

**HashedExpression**, a symbolic computing library designed to work with [tensors](https://en.wikipedia.org/wiki/Tensor) (read: 1D, 2D, 3D, n-D grid of elements):
- A type-safe Haskell embedded DSL for constructing and manipulating expressions.
- A type-safe Haskell embedded DSL for defining and generating code for optimization problems.
- Doing normalization and simplification.
- Computing (partial) derivatives.
- Identifying common sub-expressions, compute executing order and generate fast C code for expressions evaluation.
This speeds up solving optimization problems because objective functions and partial derivatives share a lot in many cases.

**Symphony**, a standalone language for HashedExpression, see section [Symphony](#symphony)

HashedExpression supports some features and operations (including representing, simplifying, computing derivatives, ...) nicely that we haven't found elsewhere:
- Piecewise, useful for things like [huber loss](https://en.wikipedia.org/wiki/Huber_loss), which often used for regularization.
- Fourier transformation (representing, simplifying, computing derivatives, ...), useful for image processing problems.

## HashedExpression

TODO

## Symphony

Symphony is a standalone language backed by HashedExpression, it somewhat resembles AMPL (but free).
### Installation
We haven't yet publish it to Cabal (or Homebrew, apt-get), in the mean time, you can clone this repository and run:
```terminal
$ stack install --ghc-options -O2
```

### Usage
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
Will generate `problem.c` with the following interface:

```c++
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

...

void evaluate_partial_derivatives_and_objective() { .. } ;
void evaluate_objective() { .. };
void evaluate_partial_derivatives() { .. } ;
...
```
Which you can plug to your favorite optimization solver. Examples of
optimization solvers (LBFGS, LBFGS-b, Ipopt) using HashedExpression generated `problem.c` can be found
in `algorithms` directory, e.g: [LBFGS-b](https://github.com/dalvescb/HashedExpression/blob/master/algorithms/lbfgs-b/lbfgs-b.c).



## Contributing
Please read `Contributing.md`. PRs are welcome.


## About
The project is developed and maintained by [Dr. Christopher Anand](https://github.com/christopheranand)'s research group, Computing and Software department, McMaster University.

List of contributors:
- [Nhan Thai](https://github.com/dandoh)
- [Curtis D'alves](https://github.com/dalvescb)
- [Christopher Anand](https://github.com/christopheranand)
- ...

