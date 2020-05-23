-- MRI image reconstruction with line skipping.
-- Fourier transforms connecting the raw data which is received from imaging device as a signal, with image space.
-- The MRI raw data is not acquired in image space and the role of the image reconstruction process
-- is to transform the acquired raw data into images that can be interpreted clinically.

-- Signal = (Re, Im) = Re + i Im

-- The signal data may be reconstructed in several ways: (1) as a "real" image, (2) as an "imaginary" image,
-- (3) as a magnitude image, or (4) as a phase image.

-- In this implementation, reconstruction is done as a "real" image and we skip the information from imaginary part.
-- For this reason, one can split the MRI image in k-space into real and imaginary parts, and just take the
-- real part for reconstruction. 

-- Putting a thereshold here, real part of k-space can be shown in binary 
-- (signal = abs(re) > 0.5), so if we have signal, signal[i][j]=1, otherwise signal[i][j]=0

-- invFFT can naively reconstruct the Brain image based on the binary form of real part of k-space

-- with box constrain, outside of the brain edge forced to zero


-- Define variable for the reconstructed image
variables:
  x[128][128] = 0

-- Define constants (imaginary, real, and signal in k-space)
constants:

  im[128][128] = Dataset("im.h5", "im")                  -- Imaginary part of the k-space
  re[128][128] = Dataset("re.h5", "re")                  -- Real part of the k-space
  signal[128][128] = Dataset("signal.h5", "signal")      -- Set the signal by using only the real part of the fourier space as the aim 
                                                         -- is to reconstruct the signal as a real image 

  xLowerBound[128][128] = Dataset("x_lb.h5", "x_lb")     -- bounderies for detecting the edge 
  xUpperBound[128][128] = Dataset("x_ub.h5", "x_ub")     
  
-- the reconstructed image is controled to be in range by these box constrains 
  x >= xLowerBound, x <= xUpperBound
  
  
-- Using Median filter for removing the noise as it preserves edges while removing noise
-- TODO (Should we put explanation of median filter? Check if the regularizer is explained correctly Curtis & Nhan)
 
let:
  smootherX = rotate (0, 1) x + rotate (0, -1) x - 2 *. x
  smootherY = rotate (1, 0) x + rotate (-1, 0) x - 2 *. x
  regularization = norm2square smootherX + norm2square smootherY
  
-- Optimization objective is to minimize the difference between the fourier transform of the reconstructed image from real part
-- and the exact fourier which is (Real + iImg). Here the signal which consists of the real part of the fourier is multiplied
-- to eliminate the effect of imaginary part in calculating the difference. The regularizer is also added to the objective as the
-- median filter reduce the noise causing by artifacts of missing signals, while it preserves the edges.
-- TODO (3000? Curtis & Nhan)
minimize:
  norm2square ((signal +: 0) * (ft x - (re +: im))) + 3000 *. regularization
