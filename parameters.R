# Clear workspace
rm(list = ls())
.rs.restartR()

# Load packages
library(rhdf5)
library(kernlab)
library(oce)
library(fda.usc)
library(seewave)
library(sfsmisc)
library(DescTools)
library(e1071)
library(PerformanceAnalytics)

# Show data structure
h5ls("X_train.h5")
h5ls("X_test.h5")

# Choose train or test data
datatype = "train"

# Import data
if (datatype == "train"){
  dataset = h5read(file="X_train.h5", name="/")
  stage = read.csv("y_train.csv", header=TRUE, row.names=1)
} else if (datatype == "test"){
  dataset = h5read(file="X_test.h5", name="/")
}

datasize = dim(dataset$eeg_1)[2]

# Extract parameters
signal_list = c("eeg_1", "eeg_2", "eeg_3", "eeg_4", "eeg_5", "eeg_6", "eeg_7", "x", "y", "z", "pulse")
param.list = list()
  
# For each signal
# EEG signals
start.time = Sys.time()
for (k in 1:7){
  signal = signal_list[k]
  param.mat = matrix(nrow = datasize, ncol = 11)
  colnames(param.mat) = c(paste0("mean_", signal), paste0("var_", signal),
                          paste0("delta_", signal), paste0("theta_", signal), paste0("alpha_", signal), 
                          paste0("beta_", signal), paste0("K_", signal),
                          paste0("theta/alpha_", signal), paste0("delta/alpha_", signal), paste0("sp_", signal),
                          paste0("zcmean_", signal)
                          )
  # For each sample
  for (i in 1:datasize){
    # Extract signal
    sample = dataset[[signal]][,i]
    Fs = 50
    sample.ts = ts(sample, start = 0, end = 30, frequency = Fs)
    # Estimate PSD via Welch's method 
    welch = pwelch(sample.ts, plot=FALSE)
    delta = integrate.xy(welch$freq, welch$spec, 0.5, 4, use.spline=TRUE)
    theta = integrate.xy(welch$freq, welch$spec, 4, 8, use.spline=TRUE)
    alpha = integrate.xy(welch$freq, welch$spec, 8, 13, use.spline=TRUE)
    beta = integrate.xy(welch$freq, welch$spec, 13, max(welch$freq), use.spline=TRUE)
    K = integrate.xy(welch$freq, welch$spec, 0.9, 1.1, use.spline=TRUE)
    total = integrate.xy(welch$freq, welch$spec, min(welch$freq), max(welch$freq), use.spline=TRUE)
    # Spectral peak
    sp = welch$freq[which.max(welch$spec)]
    # Compute zero crossing mean over 3s windows
    zc = mean(zcr(sample, f=Fs, wl=3*Fs/2, plot=FALSE)[,2])
    # Compute entropy
    #ent = Entropy(sample)

    param.mat[i,] = c(mean(sample), var(sample),
                      delta/total, theta/total, alpha/total, beta/total, K/total,
                      theta/alpha, delta/alpha, sp, zc
                      )
  }
  param.list[[k]] = param.mat
}
# Acelerometer and oxymeter signals
for (k in 8:11){
  signal = signal_list[k]
  param.mat = matrix(nrow = datasize, ncol = 9)
  colnames(param.mat) = c(paste0("mean_", signal), paste0("var_", signal), paste0("skew_", signal), paste0("kur_", signal),
                          paste0("lopwr_", signal), paste0("hipwr_", signal), paste0("totpwr_", signal), paste0("sp_", signal),
                          paste0("zcmean_", signal)
                          )
  
  # For each sample
  for (i in 1:datasize){
    # Extract signal
    sample = dataset[[signal]][,i]
    Fs = 10
    sample.ts = ts(sample, start = 0, end = 30, frequency = Fs)
    # Estimate PSD via Welch's method
    welch = pwelch(sample.ts, plot=FALSE)
    lopwr = integrate.xy(welch$freq, welch$spec, min(welch$freq), 2.5, use.spline=TRUE)
    hipwr = integrate.xy(welch$freq, welch$spec, 2.5, max(welch$freq), use.spline=TRUE)
    total = integrate.xy(welch$freq, welch$spec, min(welch$freq), max(welch$freq), use.spline=TRUE)
    # Spectral peak
    sp = welch$freq[which.max(welch$spec)]
    # Compute zero crossing mean over 3s windows
    zc = mean(zcr(sample, f=Fs, wl=3*Fs/2, plot=FALSE)[,2])
    # Compute entropy
    #ent = Entropy(sample)
    
    param.mat[i,] = c(mean(sample), var(sample), skewness(sample), kurtosis(sample),
                      lopwr/total, hipwr/total, total, sp, zc
                      )
  }
  param.list[[k]] = param.mat
}
end.time = Sys.time()
print(end.time - start.time)

# Save parameters
if (datatype == "train"){
  train.parameters = data.frame(param.list[[1]], param.list[[2]], param.list[[3]], param.list[[4]], 
                                param.list[[5]], param.list[[6]], param.list[[7]],
                                param.list[[8]], param.list[[9]], param.list[[10]], param.list[[11]],
                                "stage" = as.factor(stage[,1])
                                )
  save(train.parameters, file = "trainparameters3.RData")
} else if (datatype == "test"){
  test.parameters = data.frame(param.list[[1]], param.list[[2]], param.list[[3]], param.list[[4]], 
                                param.list[[5]], param.list[[6]], param.list[[7]],
                                param.list[[8]], param.list[[9]], param.list[[10]], param.list[[11]]
                                )
  save(test.parameters, file = "testparameters3.RData")
}