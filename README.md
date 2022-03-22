# Forecasting
This repository contains a few forecasting projects written in R. Besides the general visualization and decompositions, several forecasting techniques have been applied:
* statistical methods, such as exponential smoothing, Holt, linear regression and theta
* Machine Learning methods, such as Deep Learning (Neural Network) and decision trees (XGBoost)

## Sales data
* Data visualization
* Filling missing values
* Identifying outliers + normalization
* Visualize weekly and monthly series
* Decomposition of trend and seasonal components

## Daily sales of a product sold in a store
* Data visualization 
* Some measures for forecastability are computed: ADI and CV^2
* Empirical distribution of the demand of the product and compute some percentiles of number of daily sales
* Decomposition of seasonal, trend and random components

## International airline passengers
* Decomposition in seasonal and trends components
* Generate forecasts using Simple, Holt and Damped Exponential Smoothing
* Measuring accuracy including prediction intervals

## Power generation data (wind turbine)
* Plotting some data for better understanding
* Forecasting of power generation by applying a Linear Regression Model
* Forecasting of power generation by applying a Neural Network

## Electricity prices
* Simple tests for missing values and outlier detection 
* Normalizing and imputing these
* Correlation matrix
* Decomposition
* Statistical forecasting: 
** insample/outsample data separation
** applying theta forecasting
* ML forecasting:
** scaling of data
** factorizing 
** including data lag (1 week, 2 weeks)
** insample/outsample
** define NN model and hyperparameters
** define XGBoost model and hyperparameters
* Compare scores of all models
# Forecasting
