import matplotlib.pyplot as plt
import numpy as np

def plot_data(x, y):
    plt.scatter(x, y, c='r', marker="x")
    plt.xlabel('population')
    plt.ylabel('profit')
    plt.show()


print('Plotting Data...')
X = np.loadtxt('x.txt')
y = np.loadtxt('y.txt')
m = y.size

plt.ion()
plt.figure(0)
plot_data(X, y)

input('Program paused. Press ENTER to continue')

theta0 = np.loadtxt("theta0_out.txt")
theta1 = np.loadtxt("theta1_out.txt")
theta = np.array([theta0, theta1])

# Plot the linear fit
X = np.c_[np.ones(m), X]  # Add a column of ones to X
plt.figure(0)
line1, = plt.plot(X[:, 1], np.dot(X, theta), label='Linear Regression')
plt.legend(handles=[line1])

input('ex1 Finished. Press ENTER to exit')

