import numpy as np
from matplotlib import pyplot as plt
from matplotlib import colors as cl

# read data from np
dataDict = np.load('oversample_data.npz')
dataFiles = dataDict.files
refDat = dataDict['ref']  # (256, 256, 4, 16) - 4 slices, 16 coils, complex image space

# pull out data over first slice, first coil
refK0 = refDat[:, :, 0, 0]

# show the first slice of first coil
plt.imshow(np.absolute(refK0), cmap='gray')
plt.show()

# show the k-space of the image
kspace = np.fft.fft2(refK0)
kspaceM = np.fft.fftshift(np.absolute(kspace))
plt.imshow(20 * np.log10(kspaceM), cmap='gray')
plt.show()

combined = np.sqrt((np.abs(refDat[:, :, 0, :]) ** 2).sum(axis = -1))
plt.imshow(combined, cmap='gray')
plt.show()


