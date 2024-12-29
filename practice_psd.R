install.packages("TSA")
library(signal)

# spectrum function
# Generate a sample time series (50 Hz sine wave + noise)
fs <- 1000  # Sampling frequency in Hz
t <- seq(0, 1, by = 1/fs)  # Time vector (1-second interval)
signal <- sin(2 * pi * 50 * t) + 0.5 * rnorm(length(t))  # 50 Hz sine wave + noise

# Compute PSD
psd <- spectrum(signal, log = "no")  # Set log = "no" for linear scale

# The PSD is automatically plotted


# Parameters for Welch's method
window_size <- 256
overlap <- 128

# Compute PSD using Welch's method
psd <- pwelch(signal, fs = fs, nfft = window_size, overlap = overlap)

# Plot the PSD
plot(psd$f, psd$S, type = "l", log = "y", xlab = "Frequency (Hz)", ylab = "Power Spectral Density")
