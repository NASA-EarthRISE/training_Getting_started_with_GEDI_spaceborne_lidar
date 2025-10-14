# Necessar R packages
library(data.table)
library(caret)
library(randomforest)

# input data
rf_input = read.csv("X:/XXX/Exercise_2/AGB_simulated_GEDI_RHs.csv")


# 1. Development of Randomforest AGB model with Cross-validation 
# 1.1. Setting up k-fold cross-validation
set.seed(123)
ctrl <- trainControl(
method = "repeatedcv",         # Repeated K-fold cross-validation
number = 10,                   # Number of folds
repeats = 3,                   # Number of repetitions
savePredictions = "final")     # Save predictions for analysis

# 1.2. Perform cross-validation for rf model
rf_model_cv <- train(AGB ~ .,     # Formula: outcome as a function of all other variables
data = rf_input,
method = 'rf',
ntree = 100,
trControl = ctrl,
tuneLength = 5)
print(rf_model_cv)
plot(rf_model_cv)


# 2. Apply developed RF AGB model to GEDI L2A RH Metric data

# Input GEDI L2A data (subset)
gedi_ac <- read.csv(“X:/XXXX/Exercise_2/GEDI_2A_ACRE_2019_filtered.csv")

# Apply RF model to GEDI L2A data with the same RHs.

gedi_predict <- predict(rf_model_cv, newdata = gedi_ac)

# Put "gedi_predict" together with footprint coordinates (“latitude” and “longitude”) from ‘gedi_ac’
gedi_ac$AGB <- gedi_predict
gedi_agb <- gedi_ac[, c(24:25, 27)]

# Select randomly 5,000 AGB footprints from this data for AGB prediction model on Google Earth Engine (GEE)
# as GEE can not handle large data (5,000 elements at max).
> sampled_agb_index <- sample(1:nrow(gedi_agb), size = 5000, replace = FALSE)
> sampled_agb <-gedi_agb[sampled_agb_index,]
> write.csv(sampled_agb, ‘X:/XXXX/Exercise_3/gee_agb_ac.csv')








