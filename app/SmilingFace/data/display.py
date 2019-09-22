import cv2
import numpy as np
import matplotlib.pyplot as plt
import sys

p = np.loadtxt("p.txt")
[r, c] = p.shape
print(r, c)
p = np.maximum(p, np.zeros((r, c)))
p *= (255.0/p.max())
print(p)

cv2.imshow('image',p)
cv2.waitKey(0)