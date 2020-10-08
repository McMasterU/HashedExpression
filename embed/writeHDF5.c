{
  printf("Writing %{name} to %{filePath}...\n");
  hid_t file, space, dset;
  hsize_t dims[%{shapeLength}] = { %{shape} };
  file = H5Fcreate("%{filePath}", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  space = H5Screate_simple(%{shapeLength}, dims, NULL);
  dset = H5Dcreate(file, "%{name}", H5T_IEEE_F64LE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  H5Dwrite(dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, %{address});
  H5Dclose(dset);
  H5Sclose(space);
  H5Fclose(file);
}