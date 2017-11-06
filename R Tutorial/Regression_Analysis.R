# Summary of reference below
#-----------------------------------
# Regression Analysis
#   Types of regression models
#   Simple linear regression
#   Logistic Regression
#   Building regression models
#
# Cluster Analysis
#   Types of clustering
#   Types of clusters
#   Methods to form clusters
#
# Time Series Analysis
#   Components of time series
#   Decomposition of time series - seasonal and non seasonal
#   Exponential smoothing
#   White noise
#   Correlogram
#   Box Jenkins
#   ARIMA
#-----------------------------------

# Regression Analysis
# Finding a relationship between a dependent variable
# and one or more independent variables
# Predict the value of a dependent variable based on one or more independent variables
# Coefficient explains the impact of changes in an independent variable on the dependent variable
#   
#    Y=f(X,Beta)
#     Y is the dependent variable
#     X is the independent variable
#     Beta is the unknown coefficient
# Widely used in prediction and forecasting

# Univariate and Multivariate Regression models
# Univariate Regression models use a simple or Multiple Linear model
# Simple is a single independent variate
# Multiple has two or more indep variates.

# Simple linear regression
#  Is used for three main purposes.
#  1. For describing the linear dependence of one variable on the other
#  2. For prediction of values of other variable from the one which has more data
#  3. Correction of linear depedence of one variable on the other
# A line is fitted through the group of plotted data
# The distance of the plotted points from the line gives the residual value.
# The residual value is a discrepancy between the actual and the predicted value
# The procedure to find the best fit is called the least-squares method

# Steps in building a regression model
# 1. Identify the target variables
# 2. Identify the predictors
# 3. Data Collection
# 4. Decide the relationship
# 5. Fit the model
# 6. Evaluate the model

# Linear regression model assumptions
# The predictor x is non-random.
# The error term is random
# Error term follows normal distribution
# Standard Deviation of error is independent of x
# The data being used to estimate the paramters should be independent of each other
# If any of the assumptions are violated, modelling procedure must be modified.

# Coefficient of determination R(sqr)
# A measure of goodness of fit - How well your model does fit the data?
#   R(sqr) = 0 , no linear relationship
#   R(sqr) = -1 , negative linear relationship
#   R(sqr) = +1 , positive linear relationship
# Based on R(sqr) value, we can explain how well the model explains the data and
# the % of differences that are explained by this model

# The differences between observations that are not explained by the model is the error term
# or residual
# ex:
#   R(sqr) = .74, this means that 74% of variance in the values of the 
#                 dependent variable is explained by the model and the remaining 26% is residual or error term.

# use of faithful data set
attach(faithful)
head(faithful)
# plot out the eruptions
# x=eruptions;y=waiting
plot(eruptions,waiting,xlab="Eruption duration",ylab="time waited")

# create a trendline to show corelation using the lm function (linear model)
abline(lm(waiting~eruptions))

# use case with price of ice cream and temperature relationship
# import ice cream data
data<-read.csv("icecream.csv",header = T)

# linear model creation 
attach(data)
analysis <- lm(cons~income+price+temp)
summary(analysis)
plot(analysis)


# use case using score point rating for employer
ratings<-read.csv("ratings.csv",header = T)
# perform linear regression modeling with the variables
# salary is coresponding to experience + score
fit<-lm(salary~experience+score,data=ratings)
summary(fit)


#  Call:
#  lm(formula = salary ~ experience + score, data = ratings)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-4.3586 -1.4581 -0.0341  1.1862  4.9102 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.17394    6.15607   0.516  0.61279    
#experience   1.40390    0.19857   7.070 1.88e-06 ***    <= Experience and Score both have a correlation to salary
#  score        0.25089    0.07735   3.243  0.00478 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.419 on 17 degrees of freedom
#Multiple R-squared:  0.8342,	Adjusted R-squared:  0.8147 
#F-statistic: 42.76 on 2 and 17 DF,  p-value: 2.328e-07
#-----------------------------------------------------------
# R-squared:  0.8147 means that 81% of the time the 
# variables 'salary ~ experience + score' are used


fit2<-lm(salary~experience,data=ratings)
summary(fit2)
plot(fit2)

# Use case using the PimaIndiansDiabetes2 dataset
library(mlbench)
data(PimaIndiansDiabetes2)
# remove the NAs from data
pidna <- na.omit(PimaIndiansDiabetes2)

#replace the char values in diabetes with 0 or 1
pidlm <- pidna
pidlm$diabetes<-as.numeric(pidna$diabetes)-1
head(pidlm)
test=pidlm[(301:392),]
train = pidlm[(1:300),]

lm_reg=lm(diabetes~.,data=train)
summary(lm_reg)

# predict function which will 
predicted=predict(lm_reg,newdata=test)
head(predicted)

TAB <-table(test$diabetes,predicted>0.5)
TAB
# The Mis-Classification rate can be manually determined
# using the formula
# 1 - sum(diag(TAB))/sum(TAB)
mcrate <- 1 - sum(diag(TAB))/sum(TAB)
mcrate
# 0.1521739
# 0.15 is the base line between pos and neg

# Test prediction using 0.7
TAB_high <- table(test$diabetes,predicted>0.7)
TAB_high

# Test prediction using 0.2
TAB_low <- table(test$diabetes,predicted>0.2)
TAB_low

mcrate_high <- 1 - sum(diag(TAB_high))/sum(TAB_high)
mcrate_high

mcrate_low <- 1- sum(diag(TAB_low))/sum(TAB_low)
mcrate_low

# Conclusion : The original guess of 0.5 was valid
#--------------------------------------------------------------------------------------------------
#Logistic regression
# It's a statistical method that is used in analyzing datasets where one
# or more independent variables would determine the outcome.
#  In this type of regression the dependent variables are binary, data coded as 1 for TRUE and 0 for FALSE
#  The goal of logistic regression is to find the best fitting model to describe the relationship
#    between the binary characteristic and a set of independent variables
# A Linear Regression gives a linear line as an output, once the values are plotted on a graph. Whereas, the logistic regression
# gives an S-shaped line.

#import data
mydata <-read.csv("logcs.csv",header=T)
head(mydata)
# clean the data
mydata$admit=as.factor(mydata$admit)
mydata$rank=as.factor(mydata$rank)
summary(mydata)
xtabs(~admit+rank,data = mydata)

# Using the glm Generalized Linear Model function
fit<-glm(admit ~ gre + gpa + rank,family = binomial,data=mydata)
summary(fit)

#--------------------------------------------------------------
# Cluster Analysis
#- It groups the data objects based on the information that is found in the data that
#   describes the objects in other groups
# -The goal of this procedure is that the objects in a group are similar to one another
#   and are different from the objects in other groups
# -The greater the similarity withi a group and greater the difference between the groups,
#   more distinct is the clustering.
# -Cluster Analysis provides a way for users to discover potential relationships and construct
#   systematic structures in large numbers of variables and observations.
#--------------------------------
# Types of clustering
# -Hierarchical: Also known as nesting clusters as it also clusters to exist within bigger clusters
#   to form a tree.
# -Partitioned clustering: Its simply a division of the set of data objects into non-overlapping clusters
#   such that each object is in exactly one subset.
# -Exclusive clustering: They assign each object to a single cluster.
# -Overlapping clustering: Its used to reflect the fact that an object can simultaneously belong to
#   more than one group.
# -Fuzzy clustering: Every object belongs to every cluster with a membership weight that goes
#   between 0-if it absolutely doesn't belong to that cluster 1-if it absolutely belongs to the cluster.
# -Complete clustering: It performs a hierarchical cluster analysis using a set of dissimilarities on 'n' 
#   objects that are being clustered. They tend to find compact clusters of an approximately equal diameters.
#
# Types of clusters
# -Well seperated: The distance between any two points in different groups is greater than the
#  distance between any two points within a group. They need not be globular.
# -Prototype based: The prototype of a cluster is often a centroid for data with continuous
#   attributes. Such clusters tend to be globular.
# -Graph based: When data is represented as a graph where nodes are the objects and links
#   represent connection among the objects. They tend to be globular.
# -Density based: This method is employed when the clusters are irregular and when noise and outliers are present.
# -Shared property: Also known as conceptual clustering its the process of identifying the pattern in
#  the clusters to successfully segregate into groups of clusters.

# Methods to form clusters
#
# -K means: It's a prototype based clustering technique that attemps to define the number of
#   clusters(K). They are represented as centroids.
# -Agglomerative Hierarchical Clustering: It refers to a collection of closely related clustering
#   techniques that produce a hierarchical clustering by starting with each point as singleton cluster
#   and repeatedly merging the closest clusters until a single, all encompassing cluster remains.
# -DBSCAN: A density based clustering algorithm that produces a partial clustering, in which
#   number of clusters is automatically determined by the algorithm.

# Example using simple dataset
# load data
clusterexample<-read.csv("file.csv",header=T)
clusterexample

#object Attribute.X Attribute.Y
#1      A           1           1
#2      B           2           1
#3      C           4           3
#4      D           5           4

clusterexample$object<-NULL
# k-means clustering function
# k = number of attributes:
# with 2 clusters
results<-kmeans(clusterexample,2)
results
plot(clusterexample[c("Attribute.X","Attribute.Y")],col=results$cluster)
points(results$centers,col=1:2,pch=8,cex=2)

# Example of a hierarchical cluster
# using europe info
#load data
europe<-read.csv("europe.csv",header = T)
# using the hclust function to perform a hierarchical cluster type
# load into a variable but remove the country name since it is not needed
euroclust<-hclust(dist(europe[-1]))
head(europe[-1])
# Create a dendrogram
plot(euroclust,labels=europe$Country)
# Cluster them together visually by adding rectangles around the groups
rect.hclust(euroclust,5)
rect.hclust(euroclust,4)

#--------------------------------------------------------
# Use of clustering function in R using the iris dataset
#--------------------------------------------------------

#kmeans function allows you to choose the # of K-means centroids
newiris <-iris
# exclude the Species variable
newiris$Species<-NULL
# since there are 3 Species of irises then use 3 as # of centroids
# if the K-means is not known before then trial 
kc <- kmeans(newiris,3)
kc

# create a table of values using the Species from the 3 clusters
table(iris$Species,kc$cluster)
# results
#--------------------
#1  2  3
#setosa      0 50  0
#versicolor  2  0 48
#virginica  36  0 14

# Plot the 3 clusters for visual
plot(newiris[c("Sepal.Length","Sepal.Width")],col=kc$cluster)
# accentuate the plot by using a star for centers of each cluster
points(kc$centers,col=1:3,pch=8,cex=2)
# use a dendogram to visualize the hierarchy of each cluster
hc <-hclust(dist(iris),method="ave")
# capture a sample of the iris dataset
idx <- sample(1:dim(iris)[1],40)
irisSample <- iris[idx,]

irisSample$Species <-NULL
# create a cluster of avg
hc <-hclust(dist(irisSample),method = "ave")
# Plot the sample and set the hang property to -1
plot(hc,hang = -1,labels = iris$Species[idx])

#----------------------------------------------------------------
#
# Time Series analysis
# -Time series data is an ordered sequence of observations on a quantitative variable
#    measured over an equally spaced time interval.
# -Time series are used in statistics,signal processing,pattern recognotion,econometrics,
#   mathematic finance, weather forecasting, earthquake prediction electroencephalography,
#   control engineering, astronomy, communications engineering and other places.
# -Time series analysis is
#   - a set of methods used for analyzing time series data
#   - and forecasting the future value of the variable under consideration.
# -It is assumed that the data consist of a set of identifiable components and
#   random errors which usually makes the patten difficult to identify
#
# Components of time series analysis
# Long term trend
# Seasonal variation
# Cyclical variation
# Irregular variation
#  Stationary- when the data neither increases nor decreases (completely random)
#  Non Stationary - data has some explainable portion remaining and can be analyzed further
#

# decompose() function in R is used to decompose the trend and seasonal component of a time series.
# decompose() returns a list which stores the seasonal,trend and random components of the series.
# Estimate of the seasonal component can be calculated by the decompose() function

#-------------------------------------------------------------
# Case study analysis of the time series
#
web1<-"http://rci.rutgers.edu/~rwomack/UNRATE.csv" #Unemployment rate
web2<-"http://rci.rutgers.edu/~rwomack/CPIAUCSL.csv" # inflation rates

unemploy<-read.csv(web1,row.names = 1)
head(unemploy)

inflate<-read.csv(web2,row.names = 1)
head(inflate)

class(unemploy)
# convert to time series component
urate<-ts(unemploy$VALUE,start=c(1948,1),freq=12)
head(urate)

irate<-ts(inflate$VALUE,start = c(1948,1),freq=12)
class(irate)
time(irate)
# capture a subset of the time using the 'window' function
urate.August<-window(urate,start=c(1980,8),freq=TRUE)
urate.August
plot(urate)
plot(urate.August)

# add a trendline

plot(urate)
abline(reg=lm(urate~time(urate)))
# lets decompose the trend using the function
decompose(urate)

plot(decompose(urate))

# compare the urate and irate and analyze
plot(irate,urate)
# now compare with time series
ts.plot(irate,urate,col=c("blue","red"))

#-----------------------------------------------
# Decomposing Non-Seasonal Time Series
#
#  Use Moving Average or Exponential Smoothing
#  TTR package
#   SMA - Simple moving average
#   Smooth time series using a simple moving average
#   To use the SMA() function, we need to specify the order(span) of the simple moving average, using the parameter "n"
#   To Calculate a simple moving average of order5, we set n=5 in the SMA() function.
#   Simple Exponential Smoothing
#     1 Used to make short-tem forecasts
#     2 Provides a way of estimating the level at the current time point.
#     3 Smoothing is controlled by the parameter alpha
#     4 The value of alpha lies between 0 and 1.
#     5 The values of alpha that are close to 0 mean that little weight is placed on the most recent observations.
#   Holt's Exponential Smoothing
#     1 It considers the existence of trend values in the data.
#     2 It fits the trend values non-parametrically
#     3 Smoothing is done in two ways:
#       a.Smooth between last actual data and its forecasted data
#       b.Smoothing difference between the last two forecasted values and last trend value calculated.
#   Holt - Winter Exponential Smothing
#     1 This method the model takes care of even seasonality effect
#     2 Winter's methos is also known as Holt-Winter's method of forecasting.
#     3 Assumes the multiplicative seasonal effect and additive trend components
#   EMA - Exponential Smoothing
#    1 This method is useful for making forecasts, and make no assumptions about
#       the correlations between successive values of the time series.
#    2 To make prediction intervals for forecasts using exponential smoothing methods
#       the forecast errors must be uncorrelated and normally distributed with mean zero and constant variance.
#    3 An extension and generalization of moving average smoothing technique
#    4 It continually revises a forecast in the light of more recent experience.
#    5 It smoothens out the difference between the last actual data and its forecasted value.
#   WMA - Weighted moving average
# 
#   Advantages of Exponential Smoothing
#     1 Easy to understand
#     2 Easy to implement
#     3 It's a continual learning process
#   Disadvantages
#     1 Prediction can be made only to recent future
#     2 It doesn't look at actual relationship with past
#     3 Its accuracy falls for less number of data
#--------------------------------
# Exponential smoothing and forecasting in R
#
#   ETS,ARIMA,HoltWinters(MTS package)
# ARIMA and HolWinters are used in longer forecasting(quarter year to 5 years) whereas Holt's 
#  method and simple exponential smoothing are used in shorter periods.

#Use case of monthly numeric data of cola sales
# scan  the data at url
cola1 <-scan("https://robjhyndman.com/tsdldata/data/fancy.dat")
cola1
# turn into a trend series for easier plotting. There are originally not dates.
# will add them in using the ts() funtion
cola2<-ts(cola1,freq=12,start=c(1987,1))
cola2
# now plot the series
plot.ts(cola2)
#
# Because of flutuation as well as seasonal therefore
# will transform data using the natural logrithm using log()
logcola2<-log(cola2)
plot.ts(logcola2)

# Now use the log series using HoltWinters forecast
cola2forecast<-HoltWinters(logcola2)
cola2forecast
# Will manipulate the gamma and values to 1
hw1<-HoltWinters(logcola2,beta=1,gamma=1)
hw1

hw2<-HoltWinters(logcola2,beta=1,gamma=0)
hw2

hw3<-HoltWinters(logcola2,beta=0,gamma=1)
hw3

hw4<-HoltWinters(logcola2,beta=0,gamma=0)
hw4
# The Value of alpha: 0.2320474 is very low which means that the current value
# is based upon distant values and not current.
# the lower the value of alpha the less current the data used in the values.
library(forecast)
plot(forecast(logcola2))
plot(forecast(cola2))

# Box-Jenkins forecasting Model
#   Based on statistical concepts and principles and are able to
#     model a wide spectrum of time series behavior
#   Box-Jenkins ARMA model is a combination of the AR and MA models
#   Assumes that the time series is stationary
#   Box and Jenkins recommend differencing non-stationary series one or more times to achieve
#     stationarity. Doing so produces an ARIMA model, with the "I" standing for "Integrated".
#   Some formulations transform the series by subtracting the mean of the series from each data point
#     This yields a series with a mean of zero
#   The models can be extended to include seasonal autoregressive and seasonal moving average
#     terms. Although this complicates the notation, the terms are similar to the non-seasonal
#      autoregressive and moving average terms
#   Three Primary stages in building a Box-Jenkins time series model:
#     Model Identification
#     Model Estimation
#     Model Validation
#
#   Box-Jenkins models are quite flexible due to the inclusion of both autoregressive and moving
#     average terms
#   Based on the Wold decomposition thereom, a stationary process can be approximated by an ARMA
#     model. In practice, finding that approximation may not be easy.
#   Building good ARIMA models generally requires more experience than commonly used statistical
#     methods such as regression
#   
#   ARMA (autoregressive moving average) models are used in forecasting stationary models (whose mean and variance
#     remain constant through time, i.e.,no trend effect).
#   
#   ARIMA (autoregressive integrated moving average) models are a general class of models for forecasting
#     time series of non-stationary models that can be converted to stationary models using logging
#     or differentiating.
#   If you start off with a non-stationary time series, you will first need to 'difference' the time
#     series until you obtain a stationary time series.
#   To difference a time series using R use the "diff()" function.

#------------------------------------------------------------------------------
#
# Examples of Autoregression models
# load data of a brands sales over a period 4 financial years
mydata<-read.csv("BrandData.csv")
head(mydata)
# create a time series
TS_mydata = ts(mydata$NetSales_Qty,start=c(2010,4),frequency=12)
TS_mydata
plot(TS_mydata)
#Plot has fluctuations in data making forecasting difficult
library(forecast)

model_arima<-auto.arima(TS_mydata)
model_arima
# What is the confidence of model
confint(model_arima)

#plot the new new forcast model
forecast(model_arima,20)
plot(forecast(model_arima,20))

# create the seasonal model
model_marima<-arima(TS_mydata,order = c(0,1,1),seasonal = list(order=c(0,1,1)))
model_marima

#forecast the new arima model
forecast(model_marima,20)
plot(forecast(model_marima,20))

# Create an ets model which is an automated smoothing model
model_ets<-ets(TS_mydata)
model_ets
forecast(model_ets,20)
plot(forecast(model_ets,20))

# Create a Hols-Winter model
model_hw<-HoltWinters(TS_mydata)
model_hw

forecast(model_hw,20)
plot(forecast(model_hw,20))

model_tslm<-tslm(TS_mydata~trend+season)
model_tslm

plot(forecast(model_tslm,h=20))

split.screen(figs = c(2,2))
screen(1)
plot(forecast(model_arima,20),main="Auto Arima")
screen(2)
plot(forecast(model_ets,20),main="ETS")
screen(3)
plot(forecast(model_hw,20),main="HoltWinters")
screen(4)
plot(forecast(model_tslm,h=20),main="Linear Regression")

# Cannot determine by looking at the graphs with regression to use
# will look at the AIC for all to determine
# NOTE: cannot use AIC with the HoltWinters model

AIC(model_arima,model_marima,model_ets,model_tslm)
#df      AIC
#model_arima   3 700.8074
#model_marima  3 699.2266
#model_ets    17 974.3600
#model_tslm   14 962.8930

# The ARIMA models has the least amount of data loss






























