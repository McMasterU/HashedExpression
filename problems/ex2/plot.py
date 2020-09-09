import matplotlib.pyplot as plt
import numpy as np
import os.path
from os import path

def plot_data(X, y):
    plt.figure()
    pos = np.where(y == 1)[0]
    neg = np.where(y == 0)[0]

    plt.scatter(X[pos, 0], X[pos, 1], marker="+", c='b')
    plt.scatter(X[neg, 0], X[neg, 1], marker="o", c='y')

def map_feature(x1, x2):
    degree = 6
    x1 = x1.reshape((x1.size, 1))
    x2 = x2.reshape((x2.size, 1))
    result = np.ones(x1[:, 0].shape)

    for i in range(1, degree + 1):
        for j in range(0, i + 1):
            result = np.c_[result, (x1**(i-j)) * (x2**j)]
    return result

def plot_decision_boundary(theta, X, y):
    plot_data(X[:, 1:3], y)
    # Here is the grid range
    u = np.linspace(-1, 1.5, 50)
    v = np.linspace(-1, 1.5, 50)

    z = np.zeros((u.size, v.size))

    # Evaluate z = theta*x over the grid
    for i in range(0, u.size):
        for j in range(0, v.size):
            z[i, j] = np.dot(map_feature(u[i], v[j]), theta)

    z = z.T

    # Plot z = 0
    # Notice you need to specify the range [0, 0]
    cs = plt.contour(u, v, z, levels=[0], colors='r', label='Decision Boundary')
    plt.legend([cs.collections[0]], ['Decision Boundary'])

plt.ion()
X_original = np.loadtxt('x_original.txt', delimiter=' ')
y = np.loadtxt('y.txt')
plot_data(X_original, y)
plt.xlabel('Microchip Test 1')
plt.ylabel('Microchip Test 2')
plt.legend(['y = 1', 'y = 0'])
input('Program paused. Press ENTER to continue')

# X's features are all polynomial terms of X_original's features up to the sixth power
X = np.loadtxt('x_expanded.txt', delimiter=' ')

# ------------------------ RESULT ---------------------------------
if not path.exists('theta_out.txt'):
  print("Please run app/Problems/Ex2.hs and run make to produce result first...")
  exit()

theta = np.loadtxt('theta_out.txt', delimiter=' ')
lmd = 1

print('Plotting decision boundary ...')
plot_decision_boundary(theta, X, y)
plt.title('lambda = {}'.format(lmd))
plt.xlabel('Microchip Test 1')
plt.ylabel('Microchip Test 2')

input('ex2_reg Finished. Press ENTER to exit')

