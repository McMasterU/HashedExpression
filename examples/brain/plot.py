import matplotlib.pyplot as plt
import numpy as np
import h5py
import os.path
from os import path


def read_hdf5(file_name, name):
    hf = h5py.File(file_name, 'r')
    data = hf[name][:]
    hf.close()
    return data


def plot_image(data):
    plt.clf()
    plt.imshow(data, cmap="gray")


plt.ion()


print('Real part acquired by MRI...')
re = read_hdf5("kspace.h5", "re")
plot_image(np.log(np.abs(re) + 1e-10))
input('Program paused. Press ENTER to continue')

print('Imaginary part acquired by MRI...')
im = read_hdf5("kspace.h5", "im")
plot_image(np.log(np.abs(im) + 1e-10))
input('Program paused. Press ENTER to continue')

print('Naively reconstruct by taking inverse Fourier Transform...')
naive = np.abs(np.fft.ifft2(re + 1j * im))
plot_image(naive)
input('Program paused. Press ENTER to continue')

# ------------------------ RESULT ---------------------------------
if not path.exists('x_out.h5'):
  print("Please run app/Examples/Brain.hs and run make to produce result first...")
  exit()

print('Reconstructed image by optimization:')
reconstructed = read_hdf5("x_out.h5", "x")
plot_image(reconstructed)
input('Program paused. Press ENTER to continue')

