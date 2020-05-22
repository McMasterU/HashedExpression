## HashedExpression

A type-safe symbolic computing Haskell embeded DSL for solving optimization problems.

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
Which you can plug to your favorite optimization solver.  
We provide several optimization solvers (LBFGS, LBFGS-b, Ipopt) adapter in `algorithms` directory,  
e.g: [LBFGS-b](https://github.com/dalvescb/HashedExpression/blob/master/algorithms/lbfgs-b/lbfgs-b.c).

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

## Generating Haddock (with Docker)
Build the docker image located in docs (it's important you do this from the root of the repo), with
```terminal
docker build -t hashed-docker -f docs/Dockerfile .
```
-Then run the docker container to generate the haddock documentation (NOTE: every time you alter the 
-code base you'll have to rebuild the image)
```terminal
-docker run -v /some/path:/home/HashedExpression/docs hashed-docker
```
-this will generate all the haddock documentation (in html) into */some/path* on your local system

## Contributing
Please read `Contributing.md`. PRs are welcome.


## About
The project is developed and maintained by [Dr. Christopher Anand](https://github.com/christopheranand)'s research group, Computing and Software department, McMaster University.

List of contributors:
- [Nhan Thai](https://github.com/dandoh)
- [Curtis D'alves](https://github.com/dalvescb)
- [Christopher Anand](https://github.com/christopheranand)
- ...

