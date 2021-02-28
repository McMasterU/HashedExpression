import matplotlib.pyplot as plt
import numpy as np
import scipy.io as scio
import h5py

plt.ion()
np.set_printoptions(formatter={'float': '{: 0.6f}'.format})

def plot_data(X, y):
    plt.figure()

    # ===================== Your Code Here =====================
    # Instructions : Plot the positive and negative examples on a
    #                2D plot, using the marker="+" for the positive
    #                examples and marker="o" for the negative examples
    #

    pos = np.where(y == 1)[0]
    neg = np.where(y == 0)[0]

    plt.scatter(X[pos, 0], X[pos, 1], marker="+", c='b')
    plt.scatter(X[neg, 0], X[neg, 1], marker="o", c='y', s=15)

def visualize_boundary(x_min, x_max, y_min, y_max):
    h = .02
    xx, yy = np.meshgrid(np.arange(x_min, x_max, h), np.arange(y_min, y_max, h))
    print(xx.shape)
    print(yy.shape)
    print(np.c_[xx.ravel(), yy.ravel()].shape)

    # Z = clf.predict(np.c_[xx.ravel(), yy.ravel()])
    # Z = Z.reshape(xx.shape)
    Z = np.zeros(xx.shape)
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

hf = h5py.File('alpha_out.h5', 'r')
alpha = np.array(hf.get('alpha'))

print(alpha)

# def predict(v):

