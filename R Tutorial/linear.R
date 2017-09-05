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


