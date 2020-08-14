##### Neural Networks ######################

# Load the forest data as forest

install.packages("dummies")
library(dummies)
forest <- read.csv(file.choose())
View(forest)
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
forest1 <- forest[1:517,3:30]
forest1$area <- log(forest1$area+1)
View(forest1)
# apply normalization to entire data frame
forest_norm <- as.data.frame(lapply(forest1, normalize))
###
colnames(forest_norm)
# create training and test data
forest_train <- forest_norm[1:350, ]
forest_test <- forest_norm[351:517, ]
## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)
# simple ANN with only a single hidden neuron
forest_model <- neuralnet(formula = area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain + dayfri + daymon + 
                            daysat + daysun + daythu + daytue + daywed + monthapr + monthaug + monthdec + monthfeb + monthjan +
                            monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + monthsep, data = forest_train)
#
##
# visualize the network topology
plot(forest_model)
results_model <- compute(forest_model, forest_test[,-9])
str(results_model)
predicted_area <- results_model$net.result
# examine the correlation between predicted and actual values
cor(predicted_area, forest_test$area)
###0.08774534
## Improving model performance ----
# a more complex neural network topology with 5 hidden neurons
forest_model2 <- neuralnet(formula = area ~ FFMC + DMC + DC + ISI + temp + RH + wind + rain + dayfri + daymon + 
                            daysat + daysun + daythu + daytue + daywed + monthapr + monthaug + monthdec + monthfeb + monthjan +
                            monthjul + monthjun + monthmar + monthmay + monthnov + monthoct + monthsep, data = forest_train,
                          hidden = 5)
##
# visualize the network topology
plot(forest_model2)
## Evaluating model performance 
results_model2 <- compute(forest_model2, forest_test[-9])
predicted_area2 <- results_model2$net.result
cor(predicted_area2, forest_test$area)
### 0.07648866
##----
