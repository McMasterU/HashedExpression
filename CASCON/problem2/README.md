# Problem #2 - Box constraint
- Data: `re.h5` `im.h5`
- Preprocess data to generate box constraint: 
    ```shell script
    $ python3 preprocess.py
    ```
- Generate C-code from Symphony file `reconstructMRI.sp`:
    - Install Symphony by running `stack install` in HashedExpression
    (also make sure `~/.local/bin` is in your PATH).
  ```shell script
  $ symphony reconstructMRI.sp
  ```
- Compile & solve your problem:
    ```shell script
    $ make
    $ ./lbfgs-b
    ```
- Plot the result:
    ```shell script
    $ python3 plot.py x
    ```
