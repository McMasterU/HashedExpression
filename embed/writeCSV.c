  {
    printf("Writing %{name} to %{filePath}...\n");
    FILE *file = fopen("%{filePath}", "a");
    int i;
    for (i = 0; i < %{size}; i++) {
      fprintf(file, "%{name},%d,%f\n",i,*(%{address} + i));
      if (i + 1 < %{size}) fprintf(file, " ");
    }
    fclose(file);
  }
