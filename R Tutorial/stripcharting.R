# Using Stripcharts
x <- stats::rnorm(50)
xr <- round(x, 1)
oldpar<-par()
par(fg=color.id("#ff8000"))
stripchart(x) ; m <- mean(par("usr")[1:2])
text(m, 1.04, "stripchart(x, \"overplot\")")
par(fg=color.id("#00cc00"))
stripchart(xr, method = "stack", add = TRUE, at = 1.2)
text(m, 1.35, "stripchart(round(x,1), \"stack\")")
par(fg=color.id("#191970"))
stripchart(xr, method = "jitter", add = TRUE, at = 0.7)
text(m, 0.85, "stripchart(round(x,1), \"jitter\")")
par(oldpar)

stripchart(decrease ~ treatment,
           main = "stripchart(OrchardSprays)",
           vertical = TRUE, log = "y", data = OrchardSprays)

stripchart(decrease ~ treatment, at = c(1:8)^2,
           main = "stripchart(OrchardSprays)",
           vertical = TRUE, log = "y", data = OrchardSprays)

# load data from spreadsheets
w1<-read.csv(file="w1.dat",sep=",",head=TRUE)
tree<-read.csv(file="trees91.csv",sep=",",head=TRUE)

stripchart(w1$vals)
stripchart(w1$vals,method="stack")
stripchart(w1$vals,method="jitter")

# plotting vertical boxes
stripchart(w1$vals,vertical = TRUE)
stripchart(w1$vals,vertical = TRUE,method = "jitter",col = rainbow(5))

stripchart(w1$vals,method="stack",
           main="Leak BioMass in High CO2 Environment",
           xlab="BioMass of Leaves")

stripchart(w1$vals,method="stack",
           main="Leak BioMass in High CO2 Environment",
           xlab="BioMass of Leaves",bty="7")

# histogram examples
hist(w1$vals)
hist(w1$vals,main="Distribution of w1",xlab="w1")

# various breaks
hist(w1$vals,breaks=2)
hist(w1$vals,breaks=4)
hist(w1$vals,breaks=6)
hist(w1$vals,breaks=8)
hist(w1$vals,breaks=12)

# changing the domain setting xlim
hist(w1$vals,breaks=12,xlim=c(0,10))
hist(w1$vals,breaks=12,xlim=c(-1,2))
hist(w1$vals,breaks=12,xlim=c(0,2))
hist(w1$vals,breaks=12,xlim=c(1,1.3))
hist(w1$vals,breaks=12,xlim=c(0.9,1.3))

hist(w1$vals,
     main="Leak BioMass in High CO2 Environment",
     xlab="BioMass of Leaves",ylim=c(0,16))
# add the stripchart
stripchart(w1$vals,add = TRUE,at=15.5)

# histogram with a boxplot
hist(w1$vals,
     main="Leak BioMass in High CO2 Environment",
     xlab="BioMass of Leaves",ylim=c(0,16))
boxplot(w1$vals,horizontal = TRUE,add=TRUE,at=15.5,axes=FALSE)

# boxplots using the tree data
tree$C<-factor(tree$C)
tree$N<-factor(tree$N)

boxplot(tree$STBM,
        main="Stem BioMass in Different CO2 Environments",
        ylab="BioMass of Stems")
boxplot(tree$STBM~tree$CHBR)
stripchart(tree$STBM~tree$C,method="stack")
stripchart(tree$STBM~tree$C,method="stack",vertical=TRUE)
stripchart(tree$STNCC~tree$N,method="stack")

# using Scatter Plots
attach(tree)
# find corelation between Stem and Leak Biomass
plot(STBM,LFBM)
cor(STBM,LFBM)
# annotate the plot
plot(STBM,LFBM,
     main="Relationshipt Between Stem and Leaf BioMass",
     xlab="Stem Biomass",
     ylab="Leaf Biomass")

# QQ plots or quantile plot
# Used to determine if your data is close
# to being normally distributed
attach(w1)

qqnorm(vals,
       main="Normal Q-Q Plot of the Leaf Biomass",
       xlab="Theoretical Quantiles of the Leaf Biomass",
       ylab="Sample Quantiles of the Leaf Biomass",ylim=c(0,3))
stripchart(vals,add = TRUE,at=2.5)

# add stacked stripchart with trending line

qqnorm(vals,
       main="Normal Q-Q Plot of the Leaf Biomass",
       xlab="Theoretical Quantiles of the Leaf Biomass",
       ylab="Sample Quantiles of the Leaf Biomass",ylim=c(0,3))
stripchart(vals,add = TRUE,at=2.5,method="stack")
qqline(vals)


























