printf("Init value for %{name} is not provided, generate random init for %{name} ...\n");
{
  int i;
  for (i = 0; i < %{size}; i++)
  {
    ptr[%{offset} + i] = 0.2 * (double) rand() / RAND_MAX;
  }
}