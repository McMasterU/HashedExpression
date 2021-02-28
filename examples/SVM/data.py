import matplotlib.pyplot as plt
import numpy as np
import scipy.io as scio
import h5py

plt.ion()
np.set_printoptions(formatter={'float': '{: 0.6f}'.format})

def gaussian_kernel(x1, x2, sigma):
    x1 = x1.flatten()
    x2 = x2.flatten()

    sim = 0

    # ===================== Your Code Here =====================
    # Instructions : Fill in this function to return the similarity between x1
    #                and x2 computed using a Gaussian kernel with bandwith sigma
    #

    sim = np.exp(np.sum((x1 - x2) ** 2) / (-2*sigma**2))

    # ==========================================================

    return sim

data = scio.loadmat('ex6data2.mat')
X = data['X']
y = data['y'].flatten()


# print(X.shape, y.shape)
m = y.size


# kernels matrix
sigma = 2
km = np.zeros((m, m))

for i in range(m):
  for j in range(m):
    km[i][j] = gaussian_kernel(X[i], X[j], 2)

y = y + (1 - y) * (-1)
print(y)

hf = h5py.File('data.h5', 'w')
hf.create_dataset('km', data=km)
hf.create_dataset('y',data=y.reshape((m, 1)))
hf.create_dataset('alphaLB', data=np.zeros((m, 1)))
hf.create_dataset('x',data=X)
hf.close()
