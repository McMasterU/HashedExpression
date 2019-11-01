import matplotlib.pylab as plt
import numpy as np
import os
import glob
import sys
import h5py


def plot_image(var_name):
    file_name = var_name + "_out.h5"
    hf = h5py.File(file_name, 'r')
    data = hf[var_name][:]

    plt.imshow(data, cmap="gray")
    plt.show()
    hf.close()


def plot_image_real_imag(re, im):
    re_file_name = re + "_out.h5"
    hf = h5py.File(re_file_name, 'r')
    re_data = hf[re][:]

    im_file_name = im + "_out.h5"
    hf = h5py.File(im_file_name, 'r')
    im_data = hf[im][:]

    plt.imshow(np.abs(re_data + 1j * im_data), cmap="gray")
    plt.show()
    hf.close()


if (len(sys.argv) == 1):
    print("Please input the variable to plot")
elif (len(sys.argv) == 2):
    plot_image(sys.argv[1])
elif (len(sys.argv) == 3):
    plot_image_real_imag(sys.argv[1], sys.argv[2])
