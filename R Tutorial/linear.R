# learning linear regression using the 'lm' command

attach(airquality)
names(airquality)
plot(Ozone~Solar.R)

# calculation of Ozone
mean(Ozone,na.rm = TRUE)
mean.Ozone<-mean(Ozone,na.rm = TRUE)
# we can now add a Avg baseline using abline
abline(h=mean.Ozone)
# use lm to fit a regression line through these data
model1<-lm(Ozone~Solar.R)

# add the linear regression line to the plot
abline(model1,col="red")
# take the Residuals vs. Fitted
plot(model1)

termplot(model1)

#-----------------------------------------------------
# Multiple Regression  with an interaction term
#-----------------------------------------------------

plot(Ozone~Solar.R)
plot(Ozone~Wind)

coplot(Ozone~Solar.R|Wind,panel = panel.smooth)

model2<-lm(Ozone~Solar.R*Wind)

plot(model2)

summary(model2)

termplot(model2)
summary(Solar.R)

# Create values for Solar
# created looking at the Summary of Solar.R
Solar1<-mean(Solar.R,na.rm = T)
Solar2=100
Solar3=300

predict(model2,data.frame(Solar.R=100,Wind=10))

predict(model2,data.frame(Solar.R=Solar1,Wind=10))

# find prediction of various wind speeds using the Solar.R=100

p1<-predict(model2,data.frame(Solar.R=Solar1,Wind=1:20))
p2<-predict(model2,data.frame(Solar.R=Solar2,Wind=1:20))
p3<-predict(model2,data.frame(Solar.R=Solar3,Wind=1:20))

# Plot the values based on Ozone

plot(Ozone~Wind)
lines(1:20,p1)
lines(1:20,p2,col="red")
lines(1:20,p3,col="orange")

#-----------------------------------------------------
# Generalized Linear and eneralized least 
#-----------------------------------------------------
plot(Ozone~Wind,airquality)
model1<-lm(Ozone~Wind,airquality)
plot(model1)
coef(model1)

#Coefficients:
# (Intercept)         Wind  
# 96.873       -5.551  

# predictions for Wind speeds of 19 and 20 mph:
Ozone1<-coef(model1)[1]+coef(model1)[2]*19
Ozone2<-coef(model1)[1]+coef(model1)[2]*20

# Create a model using generalized lm

model2<-glm(Ozone~Wind,airquality,family=poisson)
coef(model2)

#coef(model2)
# (Intercept)        Wind 
# 5.0795877  -0.1488753 

Ozone1.glm<-exp(coef(model2)[1]+coef(model2)[2]*19)
Ozone2.glm<-exp(coef(model2)[1]+coef(model2)[2]*20)

Ozone2.glm/Ozone1.glm
#0.8616765

exp(coef(model2)[2]) #exp(-0.1488753)
#  0.8616765

library(nlme)

model3<-gls(Ozone~Wind,airquality,na.action = na.exclude)

# use the paste function to create a date with the Month & Day column.
# the airquality data represents 1973

airquality$Date<-as.Date(paste(1973,airquality$Month,airquality$Day,sep="-"))

library(lattice)

xyplot(Ozone~Date,airquality)

model4<-gls(Ozone~Wind*Date,airquality,na.action=na.exclude)
# create a subset of the data because of missing data
air2<-subset(airquality,complete.cases(Ozone))

model5<-gls(Ozone~Wind*Date,air2)

plot(ACF(model5,form=~Date),alpha=0.05)

model6<-update(model5,correlation=corAR1())

install.packages(MuMIn)
library(MuMIn)
AICc(model5,model6)
summary(model6)



























