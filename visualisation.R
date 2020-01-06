# Clear workspace
rm(list = ls())
.rs.restartR()

# Load packages
library(rhdf5)
library(kernlab)
library(qlcMatrix)

# Import labels
stage = read.csv("y_train.csv", header=TRUE, row.names = 1)

# Import training data and show structure
h5ls("X_train.h5")
h5ls("X_test.h5")

# Reading file
dataset = h5read(file="X_train.h5", name="/")

# Set vector of colours 
color = c("red", "blue", "green", "orange", "purple")

# EEG1
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_1[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG1 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_1[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_1[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG2
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_2[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG2 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_2[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_2[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG3
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_3[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG3 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_3[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_3[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG4
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_4[,istage]), type="l", col=color[i], xlab = "", ylab = "",main=paste("EEG4 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_4[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_4[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG5
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_5[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG5 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_5[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_5[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG6
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_5[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG6 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_5[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_5[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# EEG7
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:1500, rowMeans(dataset$eeg_7[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("EEG7 - stage", i))
  par(new=TRUE)
  plot(1:1500, rowMax(dataset$eeg_7[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:1500, rowMin(dataset$eeg_7[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# X
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:300, rowMeans(dataset$x[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("X - stage", i))
  par(new=TRUE)
  plot(1:300, rowMax(dataset$x[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:300, rowMin(dataset$x[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# Y
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:300, rowMeans(dataset$y[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("Y - stage", i))
  par(new=TRUE)
  plot(1:300, rowMax(dataset$y[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:300, rowMin(dataset$y[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# Z
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:300, rowMeans(dataset$z[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("Z - stage", i))
  par(new=TRUE)
  plot(1:300, rowMax(dataset$z[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:300, rowMin(dataset$z[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}

# Pulse
for (i in 1:5){
  istage = which(stage == i-1)
  plot(1:300, rowMeans(dataset$pulse[,istage]), type="l", col=color[i], xlab = "", ylab = "", main=paste("Pulse - stage", i))
  par(new=TRUE)
  plot(1:300, rowMax(dataset$pulse[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
  par(new=TRUE)
  plot(1:300, rowMin(dataset$pulse[,istage]), type="l", col=color[i], lty=2, axes = FALSE, xlab = "", ylab = "")
}