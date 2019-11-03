variables:
  x11, x12, x21, x22


constants:

  CapacityFactory1 = 600000
  CapacityFactory2 = 600000

  DemandCompany1= 30000
  DemandCompany2 = 23000

  c1=1.75
  c2=2.25
  c3=2
  c4=2.50


constraints:
  x11>= 0,  x12>= 0,   x21>= 0,    x22>= 0
  x11+x21>=DemandCompany1
  x12+x22>=DemandCompany2
  x11+x12<=CapacityFactory1
  x21+x22<=CapacityFactory2




minimize:

  -((c1 * x11) + (c2 * x12) + (c3 * x21) + (c4 * x22))
