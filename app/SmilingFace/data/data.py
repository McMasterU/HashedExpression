import cv2
import numpy as np
import matplotlib.pyplot as plt
import sys

# normalize the image
img = cv2.imread('original.png', cv2.IMREAD_GRAYSCALE) / 255
head = cv2.imread('head.png', cv2.IMREAD_GRAYSCALE) / 255

(r, c) = img.shape

np.set_printoptions(threshold=sys.maxsize)
# print(img)
# print(np.max(img))
img_fft = np.fft.fft2(img)

re = np.real(img_fft)
im = np.imag(img_fft)

lost_rows = np.random.choice(r, 10)

mask = np.ones((r, c))
for i in lost_rows:
  re[i] = 0
  im[i] = 0
  mask[i] = 0

np.savetxt("original.txt", img, "%.6f")
np.savetxt("head.txt", head, "%d")
np.savetxt("re.txt", re, "%.6f")
np.savetxt("im.txt", im, "%.6f")
np.savetxt("mask.txt", mask, "%d")
