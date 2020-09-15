import matplotlib.pyplot as plt
import numpy as np
import h5py
import os.path
from os import path


def display_data(x):
    (m, n) = x.shape
    print(x.shape)

    # Set example_width automatically if not passed in
    example_width = np.round(np.sqrt(n)).astype(int)
    example_height = (n / example_width).astype(int)

    # Compute the number of items to display
    display_rows = np.floor(np.sqrt(m)).astype(int)
    display_cols = np.ceil(m / display_rows).astype(int)

    # Between images padding
    pad = 1

    # Setup blank display
    display_array = - np.ones((pad + display_rows * (example_height + pad),
                              pad + display_rows * (example_height + pad)))

    # Copy each example into a patch on the display array
    curr_ex = 0
    for j in range(display_rows):
        for i in range(display_cols):
            if curr_ex > m:
                break

            # Copy the patch
            # Get the max value of the patch
            max_val = np.max(np.abs(x[curr_ex]))
            display_array[pad + j * (example_height + pad) + np.arange(example_height),
                          pad + i * (example_width + pad) + np.arange(example_width)[:, np.newaxis]] = \
                          x[curr_ex].reshape((example_height, example_width)) / max_val
            curr_ex += 1

        if curr_ex > m:
            break

    # Display image
    plt.figure()
    plt.imshow(display_array, cmap='gray', extent=[-1, 1, -1, 1])
    plt.axis('off')

plt.ion()
input_layer_size = 400  # 20x20 input images of Digits
hidden_layer_size = 25  # 25 hidden layers
num_labels = 10         # 10 labels, from 0 to 9

print('Loading and Visualizing Data ...')

hf = h5py.File('data.h5', 'r')
X = np.array(hf.get('x'))
y = np.array(hf.get('y'))
(m, _) = y.shape


# Randomly select 100 data points to display
rand_indices = np.random.permutation(range(m))
selected = X[rand_indices[0:100], :]

display_data(selected)

input('Program paused. Press ENTER to continue')

if (not path.exists('theta1_out.h5')) or (not path.exists('theta2_out.h5')) :
  print("Please run app/Examples/NeuralNetwork.hs and run make to produce result first...")
  exit()

print('Loading Trained Neural Network Parameters ...')

theta1_f = h5py.File('theta1_out.h5', 'r')
theta1 = np.array(theta1_f.get('theta1'))

theta2_f = h5py.File('theta2_out.h5', 'r')
theta2 = np.array(theta2_f.get('theta2'))

print(theta1.shape)
print(theta2.shape)

print('Visualizing Neural Network...')

display_data((theta1.T)[:, 1:])

input('Program paused. Press ENTER to continue')


def sigmoid(z):
    return 1 / (1 + np.exp(-z))

def predict(theta1, theta2, x, y):
    m = x.shape[0]

    x = np.c_[np.ones(m), x]
    h1 = sigmoid(np.dot(x, theta1))
    h1 = np.c_[np.ones(h1.shape[0]), h1]
    h2 = sigmoid(np.dot(h1, theta2))
    p = np.argmax(h2, axis=1) + 1
    actual = np.argmax(y, axis=1) + 1

    return np.mean(p == actual) * 100

pred = predict(theta1, theta2, X, y)

print('Training set accuracy: {}'.format(pred))

input('ex4 Finished. Press ENTER to exit')
