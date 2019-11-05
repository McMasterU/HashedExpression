param a;
param b;
param c;
param n;
param m;

var x >= n;

minimize Obj:  a * x*x + b * x + c;

subject to G:
  x * x <= m;
