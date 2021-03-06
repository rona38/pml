Prediction Assignment Writeup
========================================================

## Introduction

This is course project assignment of Practical Machine Learning. 

## Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

## Goal

The goal of this project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. The exercises are categorized in classe: A, B, C, D, E.
Any other variables are allowed to be used as features to predict which exercises are measured.
The prediction model must be able to predict 20 different test cases.


## Analysis

### Training data

The training data consists of 160 variables that are obtains from the accelerometers on the belt,forearm, arm and dumbbel of the participants. Many of the columns has NA or empty value. The first step will be discarding those columns. The remaining columns is about 60 columns. The next step is removing the columns that are not a accelerometer measurement. That is the first 7 columns.The remaining 53 columns will be used in the training data. The training data are splits into 70% training data and 30% cross validation data.


### Prediction.

To get the best predictor comparison has been made with differents methods. The accuracy of each prediction models is compared to each other. The selected prediction models are:

1. CART
2. RandomForest.
3. Generalized Boosting model
4. Model prediction model.

### Prediction model Accuracy

The prediction models validated tested again the cross validation data. The following table shows the accuracy of each model. The results are obtained by calling the Confusion matrix again the cross validation data.


 Prediction model           | Accuracy          
 ---------------------------|------------
 CART                       | 50.29%              
 RandomForest               | 100.0%                  
 Generalized Boosting model | 97.29%              
 Model prediction model     | 69.91%              

### The Winner is RandomForest

Based on the result we get from the evaluation. The random forest method is used to predict the test set. The result is 20/20 as according to the evaluation.
