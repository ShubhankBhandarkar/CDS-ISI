# -*- coding: utf-8 -*-
"""
Created on Sat Sep 16 18:41:39 2017

@author: Group2
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# ======================================================================================================================
# Problem 1
# Batch Gradient Descent

# Here we are providing input to the BatchGradientDescent function:
# Dataset is split into 2 with 1 dependent variable and 2nd rest all the data
# Other inputs are LearningRate, theta and accuracy
def BatchGradientDescent(data_dep, data_indep, learnRate, theta, accuracy):
    n = len(data_dep)
    J = count = 0
    result = pd.DataFrame([])
    acc_check = 1
    J = (1.0/(2*n)) * sum([(sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])**2 for i in range(n)])
    while (acc_check > accuracy):
        theta_dummy = theta.copy()
        for j in range(0,len(theta)):
            for i in range(0,n):
                lms_sum = 0
                lms_sum += (sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])*data_indep.iloc[i,j]
            theta_dummy[j] = theta[j] - learnRate * (lms_sum/len(data_dep))
        theta = theta_dummy.copy()
        J_theta = 0
        J_theta = (1.0/(2*n)) * sum([(sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])**2 for i in range(n)])
        count += 1
        result = result.append(pd.DataFrame({'count': count, 'J_Theta':J_theta}, index=[0]), ignore_index = True)
        acc_check = abs(J_theta-J)
        J = J_theta.copy()
    plt.plot(result.iloc[:,0])
    return theta

# Here we have used advertisement data for analysis
data = pd.read_csv("http://souravsengupta.com/cds2017/lectures/Advertising.csv")
# Separating the dependent and independent dataframes
data_indep = data.iloc[:,0:4]
data_indep.iloc[:,0] = 1
data_dep = data.iloc[:,4]
theta = np.array([3.0,0.5, 0.2, 0.0])
learnRate = 0.0005
accuracy = 0.0000001
result = BatchGradientDescent(data_dep, data_indep, learnRate, theta, accuracy)
print(result)

#================================================================================================
# Problem 2
# Stochastic Gradient Descent

# Data structure is same as that of BatchGradientDescent
def StochasticGradientDescent(data_dep, data_indep, learnRate, theta, accuracy):
    n = len(data_dep)
    J = count = 0
    result = pd.DataFrame([])
    acc_check = 1
    J = (1.0/(2*n)) * sum([(sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])**2 for i in range(n)])
    while (acc_check > accuracy):
        theta_dummy = theta.copy()
        for j in range(0,len(theta)):
            for i in range(0,n):
                loss = (sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])*data_indep.iloc[i,j]
                theta_dummy[j] = theta_dummy[j] - learnRate * loss
        theta = theta_dummy.copy()
        J_theta = 0
        J_theta = (1.0/(2*n)) * sum([(sum(data_indep.iloc[i]*theta) - data_dep.iloc[i])**2 for i in range(n)])
        count += 1
        result = result.append(pd.DataFrame({'count': count, 'J_Theta':J_theta}, index=[0]), ignore_index = True)
        acc_check = abs(J_theta-J)
        J = J_theta.copy()
    plt.plot(result.iloc[:,0])
    return theta

# Same data structure as problem 1
data = pd.read_csv("http://souravsengupta.com/cds2017/lectures/Advertising.csv")
data_indep = data.iloc[:,0:4]
data_indep.iloc[:,0] = 1
data_dep = data.iloc[:,4]
theta = np.array([3.0,0.5, 0.2, 0.0])
learnRate = 1e-7
accuracy = 0.001
Stoch_result = StochasticGradientDescent(data_dep, data_indep, learnRate, theta, accuracy)
print(Stoch_result)
