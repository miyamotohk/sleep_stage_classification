# Clear workspace
rm(list = ls())
.rs.restartR()

# Load packages
library(rhdf5)
library(kernlab)
library(randomForest)

# TRAINING
# Import train data parameters
load("trainparameters3.RData")

# Apply kernel SVM
start.time = Sys.time()
fit.svm = ksvm(stage ~ ., data=train.parameters, kernel="rbfdot", type="C-svc", kpar="automatic", C=1, cross=5)
end.time = Sys.time()
print(end.time - start.time)
print(fit.svm)

# Apply random forest
start.time = Sys.time()
fit.rf = randomForest(stage ~ ., data=train.parameters, proximity=FALSE, importance=FALSE, ntree=1000, mtry=15)
end.time = Sys.time()
print(end.time - start.time)
print(fit.rf)

save(fit.svm, file = "fitsvm.gaussian3")
save(fit.rf, file = "randomforest3")

# EXPLORING RESULTS
print(fit.rf)
# Plot method for randomForest objects
plot(fit.rf, main="Erreur de classification")
# Extract variable importance measure
rf.importance = importance(fit.rf)
# Multi-dimensional Scaling Plot of Proximity matrix from randomForest
MDSplot(fit.rf, train.parameters$stages)

####################################################
# OOB estimate of  error rate: 21.88% randomforest3
# Cross validation error : 0.250324  fitsvm.gaussian3
####################################################

# PREDICTION
# Import model
load("fitsvm.gaussian3")
load("randomforest3")
# Import test data
load("testparameters3.RData")

# Predict
start.time = Sys.time()
stage.prediction = predict(fit.rf, test.parameters)
end.time = Sys.time()
print(end.time - start.time)

# Export as CSV
prediction = data.frame(index = 24688:49667, sleep_stage = as.integer(stage.prediction)-1)
write.csv(prediction, "prediction.csv", row.names = FALSE)