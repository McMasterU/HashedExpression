{
  printf("Reading %{name} from %{dataset} of HDF5 file %{filePath} ...\n");
  hid_t file, dset;
  file = H5Fopen("%{filePath}", H5F_ACC_RDONLY, H5P_DEFAULT);
  dset = H5Dopen(file, "%{dataset}", H5P_DEFAULT);
  H5Dread(dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, %{address});
  H5Fclose(file);
  H5Dclose(dset);
}