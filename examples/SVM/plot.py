import matplotlib.pyplot as plt
import numpy as np
import scipy.io as scio
import h5py

plt.ion()
np.set_printoptions(formatter={'float': '{: 0.6f}'.format})

def plot_data(X, y):
    plt.figure()
    pos = np.where(y == 1)[0]
    neg = np.where(y == -1)[0]

    plt.scatter(X[pos, 0], X[pos, 1], marker="+", c='b')
    plt.scatter(X[neg, 0], X[neg, 1], marker="o", c='y', s=15)

# print(np.c_[1, 2, 3, 4])
def visualize_boundary(w, b, x_min, x_max, y_min, y_max):
    h = .02
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    # pts = np.array([xx.ravel(), yy.ravel()])
    # print(w.shape, pts.shape)
    # Z = np.reshape(np.matmul(w, pts) + b, xx.shape)
    Z = np.zeros(xx.shape)
    (m, n) = xx.shape
    for i in range(m):
      for j in range(n):
        Z[i, j] = np.sum(w * np.array([xx[i, j], yy[i, j]])) + b
    plt.contour(xx, yy, Z, levels=[0], colors='r')


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

sigma = 2

datahf = h5py.File('data.h5', 'r')
y = np.array(datahf.get('y'))
x = np.array(datahf.get('x'))
alpha = np.array(datahf.get('alpha'))

(n, _) = x.shape
w = np.zeros((1, 2))
for i in range(n):
  w = w + alpha[i] * x[i]
  # np.matmul(np.transpose(y * alpha), x)

b = 0
s = 0
for i in range(n):
  if alpha[i] != 0:
    s = s + 1
    b = b + (- np.sum(w * x[i]))

b = b / s


plot_data(x, y)
visualize_boundary(w, b, 0, 4.5, 1.5, 5)
input("in")
