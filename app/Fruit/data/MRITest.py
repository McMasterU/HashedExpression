import numpy as np
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import sys
import h5py

def save_file_hdf5(numpy_data, var_name, path = "."):
    numpy_data = numpy_data.astype('float64')
    hf = h5py.File(path + "/" + var_name + ".h5", "w")
    hf.create_dataset(var_name, data=numpy_data)
    hf.close()

def rgb2gray(rgb):
    return np.dot(rgb[...,:3], [0.299, 0.587, 0.144])

# read data from np
dataDict = np.load('oversample_data.npz')
dataFiles = dataDict.files
refDat = dataDict['ref']  # (256, 256, 4, 16) - 4 slices, 16 coils, complex image space


def normalize(img):
    if (np.max(img) == 0):
        return img
    else:
        return img / np.max(img)

# pull out data over first slice, first coil
img = refDat[:, :, 0, 0]
abs_img = np.absolute(img)
# plt.imshow(normalize(abs_img), cmap='gray')
# plt.show()



# show the k-space of the image
kspace = np.fft.fft2(img)
kspaceM = np.fft.fftshift(np.absolute(kspace))
# plt.imshow(20 * np.log10(kspaceM), cmap='gray')
# plt.show()

# show combined of coils (of the 1st slice)
combined = np.sqrt((np.abs(refDat[:, :, 0, :]) ** 2).sum(axis = -1))
# plt.imshow(combined, cmap='gray')
# plt.show()

fruits = rgb2gray(plt.imread("fruits.png"))
re = kspace.real
im = kspace.imag

save_file_hdf5(fruits, "fruits")
save_file_hdf5(re, "re")
save_file_hdf5(im, "im")
