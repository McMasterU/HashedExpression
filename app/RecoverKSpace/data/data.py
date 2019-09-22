import cv2
import numpy as np
import matplotlib.pyplot as plt
import sys

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

for i in range(1, r):
  if (i % 5 == 0):
    re[i] = 0
    im[i] = 0
    mask[i] = 0


np.savetxt("original.txt", img, "%d")
np.savetxt("head.txt", head, "%d")
np.savetxt("re.txt", re, "%.3f")
np.savetxt("im.txt", im, "%.3f")
np.savetxt("mask.txt", mask, "%d")
