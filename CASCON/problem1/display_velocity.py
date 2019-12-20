import numpy as np
from scipy import ndimage
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import sys

np.set_printoptions(threshold=sys.maxsize)

r = 50
c = 50
vx_file = open("vx.txt", "r")
vy_file = open("vy.txt", "r")


X, Y = np.meshgrid(np.arange(0, c), np.arange(r, 0, -1))

vx = [float(val) for val in (' '.join(vx_file.readlines()).split(' '))]
vx = np.array(vx).reshape(r, c)

vy = [float(val) for val in (' '.join(vy_file.readlines()).split(' '))]
vy = np.array(vy).reshape(r, c)

color = np.sqrt(vx * vx + vy * vy)
plt.quiver(X, Y, vx, vy, color)
plt.show()
