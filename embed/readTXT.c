{
  printf("Reading %{name} from text file %{filePath} ...\n");
  FILE *fp = fopen("%{filePath}", "r");
  int i;
  for (i = 0; i < %{size}; i++){
    fscanf(fp, "%lf", %{address} + i);
  }
  fclose(fp);
}