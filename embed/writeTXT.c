  {
    printf("Writing %{name} to %{filePath}...\n");
    FILE *file;
    file = fopen("%{filePath}", "w");
    {
      int i;
      for (i = 0; i < %{size}; i++)
      {
        fprintf(file, "%f", *(%{address} + i));
        if (i + 1 < %{size})
        {
          fprintf(file, " ");
        }
      }
    }
    fclose(file);
  }