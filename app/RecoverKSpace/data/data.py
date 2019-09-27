import cv2
import numpy as np
import matplotlib.pyplot as plt
import sys
import h5py


def save_file_hdf5(numpy_data, var_name, path = "."):
    hf = h5py.File(path + "/" + var_name + ".h5", "w")
    hf.create_dataset(var_name, data=numpy_data)
    hf.close()


# normalize the image
img = cv2.imread('original.png', cv2.IMREAD_GRAYSCALE)
head = cv2.imread('head.png', cv2.IMREAD_GRAYSCALE) / 255

(r, c) = img.shape
print((r, c))

np.set_printoptions(threshold=sys.maxsize)
img_fft = np.fft.fft2(img)

re = np.real(img_fft)
im = np.imag(img_fft)

mask = np.ones((r, c))

def skip_every(n):
    return [i for i in range(1, r) if (i % n == 0)]


for i in skip_every(4):
    re[i] = 0
    im[i] = 0
    mask[i] = 0




# np.savetxt("original.txt", img, "%d")
# np.savetxt("head.txt", head, "%d")
# np.savetxt("re.txt", re, "%.3f")
# np.savetxt("im.txt", im, "%.3f")
# np.savetxt("mask.txt", mask, "%d")
save_file_hdf5(re, "re")
save_file_hdf5(im, "im")
save_file_hdf5(head, "head")
save_file_hdf5(mask, "mask")
