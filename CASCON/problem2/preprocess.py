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

def normalize(img):
    if (np.max(img) == 0):
        return img
    else:
        return img / np.max(img)

# ----------------------------------------------------
# Preprocess
# ----------------------------------------------------
def main():
    # Real and Imag
    re_file = h5py.File('re.h5', 'r')
    im_file = h5py.File('im.h5', 'r')
    re = re_file['re'][()]
    im = im_file['im'][()]
    (r, c) = re.shape
    show_img(np.log(np.abs(re) + 1))
    show_img(np.log(np.abs(im) + 1))

    # signal indicated if signal MRI is available: 1 means received, 0 means lost
    signal = abs(re) > 0.5
    show_img(signal.astype('float64'))


    # Naively reconstruct the img
    kspace = re + 1j * im
    img = np.real(np.fft.ifft2(kspace))
    show_img(img)

    # Use median filter to somewhat guess the head
    median = normalize(ndimage.median_filter(img, 10))
    head = median > 0.1
    show_img(head)

    # Make lower bound and upper bound
    bound_noise = 1
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
    save_file_hdf5(signal, "signal")
    save_file_hdf5(x_ub, "x_ub")
    save_file_hdf5(x_lb, "x_lb")


main()
