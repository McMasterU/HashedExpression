import cv2
import numpy as np
import matplotlib.pyplot as plt
import sys

p = np.loadtxt("p.txt").astype('uint8')
[r, c] = p.shape
print(r, c)

cv2.imshow('hah',p)
cv2.waitKey(0)