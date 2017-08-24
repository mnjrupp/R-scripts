
attach(mtcars)
counts<-table(gear)
barplot(counts)

#horizontal bar chart
barplot(counts,horiz = TRUE)

# add titles,legends, and colors

barplot(counts,
        main="Simple Bar Plot",
        xlab="Improvement",
        ylab="Frequency",
        legend=rownames(counts),
        col=c("red","yellow","green"))

# Stacked bar plot with colors and legends using a matrix
counts<-table(vs,gear)
barplot(counts,
        main="Car Distribution by Gears and VS",
        xlab="Number of Gears",
        col=c("grey","cornflowerblue"),
        legend=rownames(counts))
# bar plot with colors and legends using a matrix
# beside=TRUE for a side by side
#counts<-table(vs,gear)
barplot(counts,
        main="Car Distribution by Gears and VS",
        xlab="Number of Gears",
        col=c("grey","cornflowerblue"),
        legend=rownames(counts),
        beside=TRUE)

# Pie Chart example
slices<-c(10,12,4,16,8)
lbls<-c("US","UK","Australia","Germany","France")
pie(slices,labels = lbls,main="Simple Pie Chart")

# pie chart with % labeling
pct<-round(slices/sum(slices)*100)
lbls<-paste(c("US","UK","Australia","Germany","France"),"",pct,"%",sep=" ")
pie(slices,labels=lbls,col=rainbow(5),main="Pie Chart with Percentages")

# 3 dimensional pie chart
#install.packages("plotrix")
library(plotrix)
pie3D(slices,labels=lbls,explode = 0.0,
      main="3D Pie Chart")

# histogram chart
# The distribution of a continuous variable
# The frequency of scores in each bin on the 
# y-axis by dividing the range into bins on the
#x-axis

hist(mpg) # miles per gallon
# Colored histogram with different number of bins
hist(mpg,breaks=8,col="darkgreen")

#kernal Density Plot
density_data<-density(mpg)
plot(density_data)

#Filling density Plot with color

plot(density_data,main="Kernal Density of Miles Per Gallon")
polygon(density_data,col="skyblue",border="black")

# Line Charts
weight<-c(2.5,2.8,3.2,4.8,5.1,5.9,6.8,7.1,7.8,8.1)
months<-c(0,1,2,3,4,5,6,7,8,9)
plot(months,weight,type="b",
     main="Baby Weight Chart")
# whisker or boxplots are diagramsthat display the 
# distribution of data that is based on the five
# number summary
# Minimum;First Quartile;Median;Third quartile;Maximum
summary(weight)
boxplot(weight,varwidth = TRUE)

# Heat maps are 2 dimensional representations of data in which the values are
#represented by colors. The two types of heat maps are:
# Simple Heat Map : Provides immediate visual summary
# Elaborate Heat Map: Allows you to understand complex data sets
# syntax => heatmap(data,Rowv=NA,Colv=NA)

data<-read.csv("HEATMAP.csv",header = TRUE)
head(data)
# don't use the 1st column
data<-data.matrix(data[,-1])
heatmap(data,Rowv = NA,Colv=NA,col=heat.colors(256),
        scale="column")

# Word Cload 
#install.packages("wordcloud")
library(wordcloud2)
head(demoFreq)
wordcloud2(demoFreq)





































