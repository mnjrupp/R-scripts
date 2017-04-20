#data sets come from the Titanic contest @ kaggle.com 
#Load Raw data
train <- read.csv("train.csv",header=TRUE)
test <- read.csv("test.csv",header=TRUE)

#Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])

#Combine data sets
data.combined <- rbind(train,test.survived)

# A bit about R data types (e.g., factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

#load up ggplot2 package to use for visualizations
library(ggplot2)

#Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train,aes(x=Pclass,fill = factor(Survived))) +
  stat_count(width=0.5,position = position_stack(reverse = TRUE)) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Examine the first few names in the training data set
head(as.character(train$Name))


# How many unique names are there across both train and test
length(unique(as.character(data.combined$Name)))

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])

#Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]



# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g. sibsp)?
misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

males <- data.combined[which(train$Sex=="male"),]
males[1:5,]

# Expand on the relationship between Survived and PClass by adding the new Title variable
#Create a utility function to help with title extraction
extractTitle <- function(name){
  name <- as.character(name)
  if(length(grep("Miss.",name))>0){return ("Miss.")}
  else if (length(grep("Master.",name))>0){return ("Master.")}
  else if (length(grep("Mrs.",name))>0){return ("Mrs.")}
  else if (length(grep("Mr.",name))>0){return ("Mr.")}
  else {return ("Other")}
 
}

titles <- NULL
for (i in 1:nrow(data.combined)){
  titles <-c(titles,extractTitle(data.combined[i,"Name"]))
}

data.combined$title <- as.factor(titles)

#Since we only have survived lables for the train set, only use the 
# first 891 rows

ggplot(data.combined[1:891,],aes(x=title,fill = Survived)) +
  stat_count(binwidth = 0.5,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$Sex)

#Visualize the 3-way relationship of sex, pclass, and survival, compare

ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived)) +
  stat_count(width = 0.5,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

summary(data.combined$Age)

# take a look at survival rates broken out by sex, pclass
# load 'dplyr' to use 'arrange' function on fill 
library(dplyr)

ggplot(arrange(data.combined[1:891,],Survived),aes(x=Age,fill=Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10,position = position_stack(reverse = TRUE)) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count")

# Reverse the order of the 'Survived' using 'position_stack'
ggplot(arrange(data.combined[1:891,],Survived),aes(x=Age,fill=Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10,position = position_stack(reverse = TRUE)) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",],aes(x=Age,fill=Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5,position = position_stack(reverse = TRUE)) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

#Ok, appears female children may have different survival rate,
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

# turn to a factor 
data.combined$SibSp <- as.factor(data.combined$SibSp)

# title is predictive. Visualize survival rates by sisp, pclass , and title
ggplot(data.combined[1:891,],aes(x=SibSp,fill=Survived)) +
  geom_bar(width = 1,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Sibsp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the Parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,],aes(x=Parch,fill=Survived)) +
  geom_bar(width = 1,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Create a family size variable/field
temp.sibsp <- c(train$SibSp,test$SibSp)
temp.parch <- c(train$Parch,test$Parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)



# Visualize it to see if it is predictive

ggplot(data.combined[1:891,],aes(x=family.size,fill=Survived)) +
  geom_bar(width = 1,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Based on the huge number of levels ticket really isn't a factor variable it is a string
# Convert and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# THere is no immediate apparent structure in the data, let's see if we can find some
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == ""," ",substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

# We can make a factor for analysis purposes and visualize
data.combined$ticket.first.char <- as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  ggtitle("Survivability by ticket.first.char") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Ticket seems like it might be predeictive, drill down a bit
ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

#Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")


# Next up - the fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

#Can't make a fare a factor, treat as numeric and visualize with histogram

ggplot(data.combined,aes(x=Fare)) +
  geom_histogram(binwidth=5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,200)

#Let's check to see if fare has predictive power
ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived)) +
  geom_histogram(binwidth=5,position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill="Survived")

# Analysis of the cabin variable
str(data.combined$Cabin)



# Cabin really isn't a factor, make a string and display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""),"Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

# Add to combined data set and plot
data.combined$cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill = Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")
  
# Could have some predictive power, drill in
ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill = Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon pclass + title?

ggplot(data.combined[1:891,],aes(x=cabin.first.char,fill = Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.first.char") +
  ylab("Total Count") +
  ylim(0,400) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))



ggplot(data.combined[1:891,],aes(x=cabin.multiple,fill=Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Does the survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)

# Plot data for analysis
ggplot(data.combined[1:891,],aes(x=Embarked,fill=Survived)) +
  geom_bar(position = position_stack(reverse = TRUE)) +
  facet_wrap(~Pclass + title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#=============================================================
#
# Video #4 Exploratory Modeling
#
#============================================================

# After installing the randomForest package

library(randomForest)

# Train a Rndom Forest with the default parameters using pclass & title
rf.train.1 <- data.combined[1:891,c("Pclass","title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x=rf.train.1,y=rf.label,importance = TRUE,ntree = 1000)

rf.1
varImpPlot(rf.1)

# Train a Random Forest using pclass, title & sibsp

rf.train.2 <- data.combined[1:891,c("Pclass","title","SibSp")]

set.seed(1234)
rf.2 <- randomForest(x=rf.train.2,y=rf.label,importance = TRUE,ntree = 1000)
rf.2
varImpPlot(rf.2)


# Train a Random Forest using pclass, title & parch

rf.train.3 <- data.combined[1:891,c("Pclass","title","Parch")]

set.seed(1234)
rf.3 <- randomForest(x=rf.train.3,y=rf.label,importance = TRUE,ntree = 1000)
rf.3
varImpPlot(rf.3)


# Train a Random Forest using pclass, title, sibsp & parch
rf.train.4 <- data.combined[1:891,c("Pclass","title","SibSp","Parch")]

set.seed(1234)
rf.4 <- randomForest(x=rf.train.4,y=rf.label,importance = TRUE,ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a Random Forest using pclass, title, & family.size
rf.train.5 <- data.combined[1:891,c("Pclass","title","family.size")]

set.seed(1234)
rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance = TRUE,ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using pclass,title,sibsp,& family.size
rf.train.6 <- data.combined[1:891,c("Pclass","title","SibSp","family.size")]

set.seed(1234)
rf.6 <- randomForest(x=rf.train.6,y=rf.label,importance = TRUE,ntree = 1000)
rf.6
varImpPlot(rf.6)


# Train a Random Forest using pclass,title,Parch,& family.size
rf.train.7 <- data.combined[1:891,c("Pclass","title","Parch","family.size")]

set.seed(1234)
rf.7 <- randomForest(x=rf.train.7,y=rf.label,importance = TRUE,ntree = 1000)
rf.7
varImpPlot(rf.7)


#=============================================================
#
# Video 5 - Cross Validation
#
#=============================================================


# Before we jump into features engineering we need to establish a methodology
# for estimating our error rate on the test set (i.e. unseen data). This is
# critical, for without this we are more likely to overfit. Let's start with
# a submission of rf.5 to Kaggle to see if our OOB error estimate is accurate


# Subset our test records and features
test.submit.df <- data.combined[892:1309,c("Pclass","title","family.size")]

#Make predictions
rf.5.preds <- predict(rf.5,test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df,file = "RF_SUB_20170420_1.csv",row.names = FALSE)

#Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates

library(caret)
library(doSNOW)

# Research has shown that 10-fold CV repeated 10 times is the best place to start
# however there are no hard and fast rules - this is where the experience of the 
# Data Scientist (i.e. the "art") comes into play. We'll start with 10-fold CV,
# repeated 10 times and see how it goes.

# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label,k=10,times=10)

# Check stratification
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

#Set up caret's trainControl object per above.
ctrl.1 <-trainControl(method = "repeatedcv",number = 10,repeats = 10,
                      index = cv.10.folds)

#Set up doSNOW package for multi-core training. This is helpful as we're
# going to be training a lot of trees.
# NOTE - This works on Windows and Mac, unlike doMC
cl <- makeCluster(2 , type = "SOCK")
registerDoSNOW(cl)


# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,
                   ntree = 1000,trControl = ctrl.1)







#=============================================================
#
# Video 6 - Exploratory Modeling 2
#
#=============================================================

# Let's use a single decision tree to better understand what's going on
# with our features. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand





















