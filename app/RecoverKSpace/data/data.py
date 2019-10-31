import numpy as np
from scipy import ndimage
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import sys
import h5py

np.set_printoptions(threshold=sys.maxsize)

# ----------------------------------------------------
# Helpers
# ----------------------------------------------------
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
    # normalize the image
    wanted_image = (255 * rgb2gray(plt.imread('original.png'))).astype('float64')
    (r, c) = wanted_image.shape

    # noise image
    noise_mean = 0
    noise_sigma = 10
    gaussian = np.random.normal(noise_mean, noise_sigma, (r, c))
    noised_imag = wanted_image + gaussian


    # k-space of the data
    noise_k_space = np.fft.fft2(noised_imag)
    re = np.real(noise_k_space)
    im = np.imag(noise_k_space)
    mask = np.ones((r, c))

    def skip_every(n):
        return [i for i in range(1, r) if (i % n == 0)]

    for i in skip_every(4):
        re[i] = 0
        im[i] = 0
        mask[i] = 0

    kspace = re + 1j * im
    img = np.real(np.fft.ifft2(kspace))
    show_img(img)
    median = normalize(ndimage.median_filter(img, 10))
    head = median > 0.1
    show_img(head)

    # get the region of head
    bound_noise = 2
    x_ub = np.ones((r, c)) * np.PINF
    x_lb = np.ones((r, c)) * np.NINF
    for i in range(r):
        for j in range(c):
            if not head[i, j]:
                x_ub[i, j] = bound_noise
                x_lb[i, j] = -bound_noise
            else:
                x_lb[i, j] = 0

    # Save file
    save_file_hdf5(re, "re")
    save_file_hdf5(im, "im")
    save_file_hdf5(head, "head")
    save_file_hdf5(mask, "mask")
    save_file_hdf5(x_ub, "x_ub")
    save_file_hdf5(x_lb, "x_lb")


main()