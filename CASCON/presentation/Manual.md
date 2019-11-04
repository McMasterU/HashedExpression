### Vectors and dimension
- In Symphony, everything is vector.
- Vectors
  - Dimension (Shape): can be scalar, 1D, 2D, 3D, ...
    - Scalar is just a single number 
    - 1D(n) variable is an array of n number, useful for problems in signal processing, sound processing, ..
    - 2D(m x n) variable is a 2D array of m x n numbers, useful for problems in image processing, ...
    - 3D(m x n x p) variable is a 3D array of m x n x p numbers, useful for problems in topology, image processing with voxels, ...
  - Numtype: can be real (R) or complex (C)
- We can manipulate vectors like adding, multiplying, doing inner product, ... to form new vectors (expressions).

### Forming expression
- (+) Add (point-wise) two vectors having same shape and same numtype
    - `exp1 + exp2`
- (*) Multiply (point-wise) two vectors having same shape and same numtype
    - `exp1 * exp2`
- (*.) Scale a vector with a scalar (if they form a vector space in Mathematics, i.e, real number can scale anything, 
    but complex can only scale complex)
    - `exp1 *. exp2`
- (<.>) Inner product (dot product) of two vectors 
    - Multiply point-wise, and sum over elements
    - `exp1 <.> exp2`
- (-) Substract (point-wise) two vectors having same shape and same numtype
    - `exp1 - exp2`
    - `- exp1` means (point-wise) negation of exp1
- (^) Power a vector with an integer
    - `exp^2`
- (/) Divide (point-wise) two vectors having same shape and same numtype
    - `exp1 / exp2`
- Piecewise:
    ```
    case x:
        x <= 0    -> -x
        otherwise -> x
    ```
- sumElements, norm2square, normHuber
    
    
    
### Structure
A valid symphony problem consists of:
    - Variables 
    - Objective function 
    - Constants (optional)
    - Constraints (optional)
    

### Declare variables 

We declare variables in `variables` block. 
The syntax is {VAR_NAME}{VAR_SHAPE}[= VAR_INIT].
For example:
```
variables:
    x[100][100] = 10
    y[20][20][20]
    a, b = 2, c
```
In the example above, we declare 2D variable x with shape 100x100, 3D variable y with shape 20x20x30, and 3 scalar variables a, b, c. Here x is init by 10, and b is init by 2.

If listed in the same line, variables are separate by a comma `,`. Otherwise, they should be indented equally (like in Python).

Variables without initialization will be randomly inititialized by a random number between (0, 1).

Variables can also get initial values from files or HDF5 dataset, e.g:

```
variables:
    x[100][100] = Dataset("dataset.hd5", "x_init")
    y[10][12][13] = File("y.txt")
```

### Declare constants
Similarly, we declare constants in `constants` block.
The syntax is identical to declaring varibles, except you need to provide the value for the constants (which makes sense because, you know, they're constants).
```
variables:
    real[128][128] = Dataset("dataset.hd5", "real")
    imag[128][128] = Dataset("dataset.hd5", "imag")
    delta = 10, sigma = 15
    b[10][10] = File("b.txt")
    mask[100][100] = Pattern(FIRST_ROW_1)
```

Besides specifying values from HDF5 dataset (i.e `Dataset("{FILENAME}", "{KEY}")`), reading from file (i.e `File("{FILENAME}")`), or literal (e.g `12.5`), we can also specify values of constants from pattern with syntax `Pattern({PATTERN_NAME})`. 

Some of the support patterns are:
- `FIRST_ROW_1`: The first row are 1s, and the rest are 0s

  For example, if we declare constant `m[3][3] = Pattern(FIRST_ROW_1)`, then m is:
  ```
  1 1 1 
  0 0 0
  0 0 0
  ```
- `FIRST_ROW_0`: The First row are 0s, and the rest are 1s
- `FIRST_COLUMN_0`, `FIRST_COLUMN_1`, `LAST_ROW_0`, `LAST_ROW_1`, `LAST_COLUMN_0`, `LAST_COLUMN_1`, ...

These patterns are very useful for masking.


### Objective function
Objective function is declared in `minimize` block (if you want to maximize instead, do the minimize with the negation of it). 
For example:
```
optimize:
    (x - y)^2
```
The objective function must be a scalar expression. (See section Forming expression on how to write your objective function).


### Constraints
Constraints are put in `constraints` block. If omitted, your problem is unconstrained. Otherwise, we support 2 types of constraints.
- Box constraint:
    - In the form `{VAR_NAME} (<= | >= | ==) ({CONST_NAME} | Number)`
    For example:
        ```
        variables:
            x[10][10], y[100], z
        constants:
            yUpperBound = File("y_ub.txt)
        constraints:
            x >= 10
            y <= yUpperBound
            c == 12.23
        ```
    - In the above example, every element of x is lower bound by 10, every element of y is upper bound by corresponding 
    element of yUpperBound, and c are both lower bound by 12.23 and upper bound by 12.23 (which means c == 12.23)
- Scalar constraint:
    - In the form `{SCALAR_EXPRESSION} (<= | >= | ==) ({SCALAR_CONST_NAME} | Number)`
    For example:
        ```
        variables:
            x[10][10], y[10][10], z
        constants:
            delta = 10
        constraints:
            c + 10 <= delta
            (x <.> y + z) / 2 <= delta
        ```

### Intermediate values
Sometimes your objective function can be big in terms of expression structure. In this case, we can introduce 
intermediate values in the `let` block to make things more clear.
Example:
```
let:
    regularizerX = norm2square x
    regularizerY = norm2square y
    regularizer = regularizerX + regularizerY

minimize:
    norm2square (x - y) + regularizer
```

