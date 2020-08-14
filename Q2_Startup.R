##### Neural Networks ###########################


# Load the startup data as startup
install.packages("dummies")
library(dummies)
startup <- read.csv(file.choose())
View(startup)
startup1 <- dummy.data.frame(startup, sep = ".")
View(startup1)
# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
startup_norm <- as.data.frame(lapply(startup1, normalize))
###
colnames(startup_norm)<- c("RDSpend","Admin","MarSpend","California","Florida","Newyork","Profit")
# create training and test data
startup_train <- startup_norm[1:30, ]
startup_test <- startup_norm[31:50, ]

## Training a model on the data ----
# train the neuralnet model
install.packages("neuralnet")
library(neuralnet)
# simple ANN with only a single hidden neuron
startup_model <- neuralnet(formula = Profit ~ RDSpend + Admin + MarSpend + California + Florida + Newyork,
                            data = startup_train)
##
# visualize the network topology
plot(startup_model)
results_model <- compute(startup_model, startup_test[1:6])
str(results_model)
predicted_profit <- results_model$net.result
# examine the correlation between predicted and actual values
cor(predicted_profit, startup_test$Profit)
## Improving model performance ----
# a more complex neural network topology with 10 hidden neurons
startup_model2 <- neuralnet(formula = Profit ~ RDSpend + Admin + MarSpend + California + Florida + Newyork,
                           data = startup_train,hidden = 5)
##
# visualize the network topology
plot(startup_model2)
## Evaluating model performance 
results_model2 <- compute(startup_model2, startup_test[1:6])
predicted_profit2 <- results_model2$net.result
cor(predicted_profit2, startup_test$Profit)
##----
