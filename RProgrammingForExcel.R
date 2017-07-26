setwd("F:/documents/RStudio/RProgramingForExcel")

titanic_train<-read.csv("F:/documents/RStudio/Titanic/train.csv")
str(titanic_train)

titanic_train[1:10,6]
titanic_train[2:25,6:8]

sum(titanic_train[1:10,6])

# By default the missing values which = NA are included
# unless we set the na.rm = TRUE; by default it is FALSE
sum(titanic_train[1:10,6],na.rm = TRUE)

sum(titanic_train$Age,na.rm = TRUE)

# Add a new column for the size of the family using a 
# vectorized calculation
titanic_train$FamilySize <-1+titanic_train$SibSp+
  titanic_train$Parch
View(titanic_train)

# Add a new column to the data frame
titanic_train$AgeMissing <-""
View(titanic_train)

# Update the new column to reflect reality
titanic_train$AgeMissing <- ifelse(is.na(titanic_train$Age),"Y","N")
View(titanic_train)

# Create some Descriptive analysis
# using summary
summary(titanic_train$Age)

# More summary statistics for Age column
sd(titanic_train$Age,na.rm = TRUE)
median(titanic_train$Age,na.rm = TRUE)
var(titanic_train$Age,na.rm = TRUE)
sum(titanic_train$Age,na.rm = TRUE)
length(titanic_train$Age)

# length of rows that are not blank
AgeTemp<-titanic_train$Age[!is.na(titanic_train$Age)]

# Slice Age data by Gender
FemaleAge<-titanic_train$Age[titanic_train$Sex=="female"]
summary(FemaleAge)
sd(FemaleAge,na.rm = TRUE)


MaleAge<-titanic_train$Age[titanic_train$Sex=="male"]
summary(MaleAge)
sd(MaleAge,na.rm=TRUE)

# filter and create new data-frames by Sex
female_train<-titanic_train[titanic_train$Sex=="female",]

male_train<-titanic_train[titanic_train$Sex=="male",]

# Further slice data frame by Pclass of 1

female_1st_train<-female_train[female_train$Pclass==1,]
View(female_1st_train)

male_1st_train<-male_train[male_train$Pclass==1,]
View(male_1st_train)

# Combine in a single step
female_1st_train<-titanic_train[titanic_train$Sex=="female" &
                                  titanic_train$Pclass==1,]

male_1st_train<-titanic_train[titanic_train$Sex=="male" &
                                  titanic_train$Pclass==1,]


# Subset to only the columns I'm interested in by column position
names(titanic_train)
my_subset_1<-titanic_train[,c(2,3,5,6,7,8,10,13,14)]
View(my_subset_1)

# The following code is equivalent to the my_subset_1 code
my_subset_2 <- titanic_train[,c(2:3,5:8,10,13:14)]
View(my_subset_2)

# I can also filter rows and columns by position
my_rows <- c(1:50,67,69,84,100:891)
my_features <- c(2:3,5:8,10,13:14)
my_subset_3 <- titanic_train[my_rows,my_features]
View(my_subset_3)


# Subset to only the columns I'm interested in by column name
my_features <- c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","FamilySize","AgeMissing")
my_subset_4 <- titanic_train[,my_features]
View(my_subset_4)

# The following code is equivalent to the my_subset_4 code
my_features <- names(titanic_train)
my_features <- my_features[-c(1,9,11,12)]
my_subset_5 <- titanic_train[,my_features]
View(my_subset_5)

# Subset to only the columns I'm interested in
my_rows <- c(51:66,68,70:83,85:99)
my_features <- names(titanic_train)[-c(1,9,11,12)]
my_subset_6 <- titanic_train[-my_rows,my_features]
View(my_subset_6)

# Take a look at our data and see which things are categories
str(titanic_train)

# Create a histogram of 1st class female ages
library(ggplot2)

ggplot(female_1st_train,aes(x=Age)) +
  geom_histogram(binwidth = 5)





































