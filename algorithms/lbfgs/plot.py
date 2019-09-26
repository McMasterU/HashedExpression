import matplotlib.pylab as plt
import numpy as np
import os
import glob
import sys
import h5py


def plot_image(var_name):
    file_name = var_name + ".h5"
    hf = h5py.File(file_name, 'r')
    data = hf[var_name]
    plt.imshow(data, cmap = "gray")
    plt.show()
    hf.close()


if (len(sys.argv) == 1):
    print("Please input the variable to plot")
else:
    plot_image(sys.argv[1])
