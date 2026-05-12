import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import scipy as scipy
from scipy import optimize
from matplotlib.ticker import AutoMinorLocator
from matplotlib import gridspec
import matplotlib.ticker as ticker

def abline(slope, intercept):
    axes = plt.gca()
    x_vals = np.array(axes.get_xlim())
    y_vals = intercept + slope * x_vals
    plt.plot(x_vals, y_vals, '--')
    
# read data
espectro = np.loadtxt("spec157164.dat")

# creating some noise to add the the y-axis data
wave = espectro[:,0]
flux = espectro[:,1]
fssp = espectro[:,2]

plt.figure(figsize=(8,6), dpi=150)
plt.plot(wave, flux)
plt.plot(wave, fssp)
plt.show()

plt.figure(figsize=(8,6), dpi=150)
plt.plot(wave, flux-fssp)
abline(0, 0)
plt.xlim(4800.0,5080.0)
plt.show()

plt.figure(figsize=(8,6), dpi=150)
plt.plot(wave, flux-fssp)
abline(0, 0)
plt.xlim(4970.0,5030.0)
plt.ylim(-0.5,7.0)
plt.show()

double01 = np.logical_and(wave > 4970.0, wave < 5030.0)
x_array = wave[np.where(double01)]
y_array_gauss = flux[np.where(double01)] - fssp[np.where(double01)]

###############################################################################

amp1 = 4.0
sigma1 = 180.0
cen1 = 5005.0

def _1gaussian(x, amp1,cen1,sigma1):
    return amp1*(1/(sigma1*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen1)/sigma1)**2)))

popt_gauss, pcov_gauss = scipy.optimize.curve_fit(_1gaussian, x_array, y_array_gauss, p0=[amp1, cen1, sigma1])

perr_gauss = np.sqrt(np.diag(pcov_gauss))

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, y_array_gauss, "ro")
plt.plot(x_array, _1gaussian(x_array, *popt_gauss), 'k--')
plt.show()

# this cell prints the fitting parameters with their errors
print(popt_gauss[0], perr_gauss[0])
print(popt_gauss[1], perr_gauss[1])
print(popt_gauss[2], perr_gauss[2])

residual_1gauss = y_array_gauss - (_1gaussian(x_array, *popt_gauss))

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, residual_1gauss, "bo")
abline(0, 0)
abline(0, np.mean(residual_1gauss))
abline(0, np.mean(residual_1gauss)+np.std(residual_1gauss))
abline(0, np.mean(residual_1gauss)-np.std(residual_1gauss))
plt.show()

###############################################################################

amp1 = 4.0
sigma1 = 180.0
cen1 = 5005.0

amp2 = 4.0
sigma2 = 180.0
cen2 = 5000.0

def _2gaussian(x, amp1,cen1,sigma1, amp2,cen2,sigma2):
    return amp1*(1/(sigma1*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen1)/sigma1)**2))) + \
            amp2*(1/(sigma2*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen2)/sigma2)**2)))

popt_2gauss, pcov_2gauss = scipy.optimize.curve_fit(_2gaussian, x_array, y_array_gauss, p0=[amp1, cen1, sigma1, amp2, cen2, sigma2])

perr_2gauss = np.sqrt(np.diag(pcov_2gauss))

pars_1 = popt_2gauss[0:3]
pars_2 = popt_2gauss[3:6]
gauss_peak_1 = _1gaussian(x_array, *pars_1)
gauss_peak_2 = _1gaussian(x_array, *pars_2)

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, y_array_gauss, "ro")
plt.plot(x_array, _2gaussian(x_array, *popt_2gauss), 'k--')
plt.show()

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, y_array_gauss, "ro")
plt.plot(x_array, _2gaussian(x_array, *popt_2gauss), 'k--')
plt.plot(x_array, gauss_peak_1, "g")
plt.plot(x_array, gauss_peak_2, "y")
plt.show()

# this cell prints the fitting parameters with their errors
print(pars_1[0], perr_2gauss[0])
print(pars_1[1], perr_2gauss[1])
print(pars_1[2], perr_2gauss[2])
print(pars_2[0], perr_2gauss[3])
print(pars_2[1], perr_2gauss[4])
print(pars_2[2], perr_2gauss[5])

residual_2gauss = y_array_gauss - (_2gaussian(x_array, *popt_2gauss))

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, residual_2gauss, "bo")
abline(0, 0)
abline(0, np.mean(residual_2gauss))
abline(0, np.mean(residual_2gauss)+np.std(residual_2gauss))
abline(0, np.mean(residual_2gauss)-np.std(residual_2gauss))
plt.show()

###############################################################################

amp1 = 4.0
sigma1 = 180.0
cen1 = 5005.0

amp2 = 4.0
sigma2 = 180.0
cen2 = 5001.0

amp3 = 4.0
sigma3 = 180.0
cen3 = 4997.0

def _3gaussian(x, amp1,cen1,sigma1, amp2,cen2,sigma2, amp3,cen3,sigma3):
    return amp1*(1/(sigma1*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen1)/sigma1)**2))) + \
            amp2*(1/(sigma2*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen2)/sigma2)**2))) + \
            amp3*(1/(sigma3*(np.sqrt(2*np.pi))))*(np.exp((-1.0/2.0)*(((x_array-cen3)/sigma3)**2)))

popt_3gauss, pcov_3gauss = scipy.optimize.curve_fit(_3gaussian, x_array, y_array_gauss, p0=[amp1, cen1, sigma1, amp2, cen2, sigma2, amp3, cen3, sigma3])

perr_3gauss = np.sqrt(np.diag(pcov_3gauss))

pars_1 = popt_3gauss[0:3]
pars_2 = popt_3gauss[3:6]
pars_3 = popt_3gauss[6:9]
gauss_peak_1 = _1gaussian(x_array, *pars_1)
gauss_peak_2 = _1gaussian(x_array, *pars_2)
gauss_peak_3 = _1gaussian(x_array, *pars_3)

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, y_array_gauss, "ro")
plt.plot(x_array, _3gaussian(x_array, *popt_3gauss), 'k--')
plt.show()

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, y_array_gauss, "ro")
plt.plot(x_array, _3gaussian(x_array, *popt_3gauss), 'k--')
plt.plot(x_array, gauss_peak_1, "g")
plt.plot(x_array, gauss_peak_2, "y")
plt.plot(x_array, gauss_peak_3, "m")
plt.show()

# this cell prints the fitting parameters with their errors
print(pars_1[0], perr_3gauss[0])
print(pars_1[1], perr_3gauss[1])
print(pars_1[2], perr_3gauss[2])
print(pars_2[0], perr_3gauss[3])
print(pars_2[1], perr_3gauss[4])
print(pars_2[2], perr_3gauss[5])
print(pars_3[0], perr_3gauss[6])
print(pars_3[1], perr_3gauss[7])
print(pars_3[2], perr_3gauss[8])

residual_3gauss = y_array_gauss - (_3gaussian(x_array, *popt_3gauss))

plt.figure(figsize=(8,6), dpi=150)
plt.plot(x_array, residual_3gauss, "bo")
abline(0, 0)
abline(0, np.mean(residual_3gauss))
abline(0, np.mean(residual_3gauss)+np.std(residual_3gauss))
abline(0, np.mean(residual_3gauss)-np.std(residual_3gauss))
plt.show()
