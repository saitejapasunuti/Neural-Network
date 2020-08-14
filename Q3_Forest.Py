############# Neural Network ###########

import numpy as np
import pandas as pd
# Importing necessary models for implementation of ANN
from keras.models import Sequential
from keras.from keras.layers import Dense #, Activation,Layer,Lambda
from sklearn.model_selection import train_test_split
#
forest = pd.read_csv("E:\\Data_Science\\Datasets\\NN\\fireforests.csv")
forest.head()
forest.columns
forest.shape
###checking the nulls
forest.isnull().sum()
###removing the unncesary columns because already has dummy columns 
forest1 = forest.iloc[:, 2:]
forest1.head()
forest1.columns
##changing the order of the columns
forest2 = forest1[['FFMC', 'DMC', 'DC', 'ISI', 'temp', 'RH', 'wind', 'rain', 
       'dayfri', 'daymon', 'daysat', 'daysun', 'daythu', 'daytue', 'daywed',
       'monthapr', 'monthaug', 'monthdec', 'monthfeb', 'monthjan', 'monthjul',
       'monthjun', 'monthmar', 'monthmay', 'monthnov', 'monthoct', 'monthsep','area']]
forest2.shape
forest2.head()
#forest.area.value_counts().plot(kind="bar")
def prep_model(hidden_dim):
    model = Sequential()
    for i in range(1,len(hidden_dim)-1):
        if (i==1):
            model.add(Dense(hidden_dim[i],input_dim=hidden_dim[0],kernel_initializer="normal",activation="relu"))
        else:
            model.add(Dense(hidden_dim[i],activation="relu"))
    # for the output layer we are not adding any activation function as 
    # the target variable is continuous variable 
    model.add(Dense(hidden_dim[-1]))
    # loss ---> loss function is means squared error to compare the output and estimated output
    # optimizer ---> adam
    # metrics ----> mean squared error - error for each epoch on entire data set 
    model.compile(loss="mean_squared_error",optimizer="adam",metrics = ["mse"])
    return (model)
##
column_names = list(forest2.columns)
predictors = column_names[0:27]
target = column_names[27]

first_model = prep_model([27,50,1])
first_model.fit(np.array(forest2[predictors]),np.array(forest2[target]),epochs=10)
pred_train = first_model.predict(np.array(forest2[predictors]))
pred_train = pd.Series([i[0] for i in pred_train])
rmse_value = np.sqrt(np.mean((pred_train-forest2[target])**2))
import matplotlib.pyplot as plt
plt.plot(pred_train,forest2[target],"bo")
np.corrcoef(pred_train,forest2[target]) # we got high correlation 
