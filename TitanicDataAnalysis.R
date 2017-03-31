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
  stat_count(width=0.5) +
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
  stat_count(binwidth = 0.5) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$Sex)

#Visualize the 3-way relationship of sex, pclass, and survival, compare

ggplot(data.combined[1:891,],aes(x=Sex,fill=Survived)) +
  stat_count(width = 0.5) +
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
  geom_histogram(binwidth = 10) +
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

















