import numpy as np
from scipy import ndimage
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import sys
import h5py



# Helpers
def save_file_hdf5(numpy_data, var_name, path="."):
    numpy_data = numpy_data.astype('float64')
    hf = h5py.File(path + "/" + var_name + ".h5", "w")
    hf.create_dataset(var_name, data=numpy_data)
    hf.close()

def show_img(data):
    plt.imshow(data, cmap='gray')
    plt.show()


def rgb2gray(rgb):
    return np.dot(rgb[..., :3], [0.299, 0.587, 0.144])


def normalize(img):
    if (np.max(img) == 0):
        return img
    else:
        return img / np.max(img)


def main():
    hihi = np.array([1, 2, -np.inf, -np.inf]);
    save_file_hdf5(hihi, "hihi")
    return
    # read data from np
    dataDict = np.load('oversample_data.npz')
    dataFiles = dataDict.files
    refDat = dataDict['ref']  # (256, 256, 4, 16) - 4 slices, 16 coils, complex image space

    # pull out data over first slice, first coil
    img = refDat[:, :, 0, 0]
    abs_img = np.absolute(img)

    # print(np.max(img.real)) # 2643212 -- 2.6e6


    # show the k-space of the image
    kspace = np.fft.fft2(img)
    kspaceM = np.fft.fftshift(np.absolute(kspace))

    # show combined of coils (of the 1st slice)
    combined = np.sqrt((np.abs(refDat[:, :, 0, :]) ** 2).sum(axis=-1))

    real_medianed = np.zeros((256, 256, 16))
    imag_medianed = np.zeros((256, 256, 16))
    total_medianed = real_medianed + 1j * imag_medianed
    for i in range(16):
        real_medianed[:, :, i] = ndimage.median_filter(refDat[:, :, 0, i].real, 10)
        imag_medianed[:, :, i] = ndimage.median_filter(refDat[:, :, 0, i].imag, 10)
        total_medianed[:, :, i] = real_medianed[:, :, i] + 1j * imag_medianed[:, :, i]


    median_combined = np.sqrt((np.abs(total_medianed) ** 2).sum(axis=-1))
    show_img(median_combined)



    save_file_hdf5(combined, "c")
    save_file_hdf5(img.real, "img")
    # save_file_hdf5(median_denoised, "median")

    # res = combined / median_denoised
    # plt.subplot(1, 2, 1)
    # plt.imshow(combined, cmap='gray')
    # plt.subplot(1, 2, 2)
    # plt.imshow(median_denoised, cmap='gray')
    plt.show()

    # write files
    # fruits = rgb2gray(plt.imread("fruits.png"))
    # re = kspace.real
    # im = kspace.imag
    # save_file_hdf5(fruits, "fruits")
    # save_file_hdf5(re, "re")
    # save_file_hdf5(im, "im")

main()
