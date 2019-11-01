import numpy as np
from scipy import ndimage
from matplotlib import pyplot as plt
from matplotlib import colors as cl
import sys
import h5py
import math

np.set_printoptions(threshold=sys.maxsize)

# ----------------------------------------------------
# Helpers
# ----------------------------------------------------
def save_file_hdf5(numpy_datas, file_name, data_names, path="."):
    hf = h5py.File(path + "/" + file_name + ".h5", "w")
    for (data,name) in zip(numpy_datas,data_names):
        data = data.astype('float64')
        hf.create_dataset(name, data=data)
    hf.close()


def show_img(data):
    plt.imshow(data, cmap='gray')
    plt.show()

def normalize(img):
    if (np.max(img) == 0):
        return img
    else:
        return 255 * (img / np.max(img))

# ----------------------------------------------------
# Preprocess
# ----------------------------------------------------
def main():
    dataDict = np.load('/home/dalvescb/data/oversample_data.npz')
    dataFiles = dataDict.files
    refDat = dataDict['ref']

    coil0 = normalize(np.fft.fft2(refDat[:,:,0,0]))
    coil1 = normalize(np.fft.fft2(refDat[:,:,0,2]))
    coil2 = normalize(np.fft.fft2(refDat[:,:,0,4]))
    coil3 = normalize(np.fft.fft2(refDat[:,:,0,6]))
    coil4 = normalize(np.fft.fft2(refDat[:,:,0,8]))
    coil5 = normalize(np.fft.fft2(refDat[:,:,0,10]))
    coil6 = normalize(np.fft.fft2(refDat[:,:,0,12]))
    coil7 = normalize(np.fft.fft2(refDat[:,:,0,14]))


    radius = 20
    filt = np.zeros(shape=(256,256))
    for (i,j) in np.ndindex(256,256):
        filt[i,j] = math.sqrt( (128-i)**2 + (128-j)**2 ) < radius


    save_file_hdf5([coil0.imag,coil0.real,
                    coil1.imag,coil1.real,
                    coil2.imag,coil2.real,
                    coil3.imag,coil3.real,
                    coil4.imag,coil4.real,
                    coil5.imag,coil5.real,
                    coil6.imag,coil6.real,
                    coil7.imag,coil7.real,
                    filt],
                   'fruit',
                   ['im0','re0'
                    ,'im1','re1'
                    ,'im2','re2'
                    ,'im3','re3'
                    ,'im4','re4'
                    ,'im5','re5'
                    ,'im6','re6'
                    ,'im7','re7'
                    ,'filter'])

main()
