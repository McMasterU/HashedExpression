import numpy as np
from matplotlib import pyplot as plt
from matplotlib import colors as cl

# read data from np
dataDict = np.load('oversample_data.npz')
dataFiles = dataDict.files
refDat = dataDict['ref']
read4xDat = dataDict['read4x']
#dataFiles
#refDat,read4xDat
#np.shape(refDat)

# pull out data over first slice, first coil ?
refK0 = refDat[:,:,0,0]

image = np.absolute(refK0)
plt.imshow(image,cmap='gray')
plt.show()

kspace = np.fft.fft2(refK0)
kspaceM = np.fft.fftshift(np.absolute(kspace))
plt.imshow(20*np.log10(kspaceM),cmap='gray')
plt.show()

# pull out data over first slice, first coil ?
refK0 = refDat[:,:,0,0]
iK0 = np.absolute(refK0)

# IK0 = sqrt ( sum( refDat ^ 2) )
for i in range(0,256):
    for j in range (0,256):
        tmp = 0
        for x in range (0,16):
            tmp += refDat[i,j,0,x].imag**2 + refDat[i,j,0,x].real**2
        iK0[i,j] = np.sqrt(tmp)
        tmp = 0

image = np.absolute(iK0)
plt.imshow(iK0,cmap='gray')
plt.show()

refK0Real, refK0Imag = refK0.real / iK0, refK0.imag / iK0
image2 = np.absolute(refK0)
plt.imshow(image2,cmap='gray')
plt.show()

# Sum h(i) * (h(i)*p(i) - 1)^2
