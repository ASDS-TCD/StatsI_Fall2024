# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()
# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
lapply(c(),  pkgTest)

#### Problem 1
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
## visualize the data set
hist(y)

## Part 1

## get the mean and standard deviation and set the same size
meany <- mean(y)
sdy <- sd(y)
n <- 25
## set the alpha (a) value as 1-0.9 for a 90% confidence interval
a <- 0.10
## get the t-value
tvalue <- qt(1-a/2, df=n-1)
## t-value is approx 1.71
## get the margin of error
margin_error <- tvalue * (sdy/sqrt(n))
## calculate confidence intervals
lowerbound <- meany - margin_error
## lower bound is approx 93.96
upperbound <- meany + margin_error
## upper bound is approx 102.92

## verified results by using the t-test function in r
confidence_interval <- t.test(y, conf.level = 0.90)
confidence_interval
## get the lower bound
confidence_interval$conf.int[1]
## get the upper bound
confidence_interval$conf.int[2]

## results from both methods are the same. the 90% confidence
## interval is approx (93.96, 102.92)


## Part 2

## because the sample size(n) < 30, i'm going to use a one sample t-test
## in order to do this, i am assuming that the data is normally distributed
## H0 (null hypothesis) is that the mean of y is greater than 100
## setting the null hypothesis mean
null_mean <- 100
hypothesis_test_result <- t.test(y, mu= null_mean, alternative= "greater")
hypothesis_test_result
## because the p-value > 0.05, fail to reject the null hypothesis. in other words,
## there is not enough evidence to say that the class' average IQ is higher than 100 (the national average)
## can also find this out by doing calculations by hand



#### Problem 2

## Part 1
getwd
expenditure <- read.table("C:\\Users\\OTTC\\Documents\\expenditure.txt", header = TRUE, sep = "\t")
expenditure
## plotting (y,x1)
plot(expenditure$X1,expenditure$Y, main = "Scatter plot of Y versus X1", ylab= "per capita personal income in state", xlab= "per capital expenditure on shelters/housing assistance in state")
## plotting (y,x2)
plot(expenditure$X2,expenditure$Y, main = "Scatter plot of Y versus X2", ylab= "Number of residents per 100,000 that are financially insecure in state", xlab= "per capital expenditure on shelters/housing assistance in state")
## plotting (y,x3)
plot(expenditure$X3,expenditure$Y, main = "Scatter plot of Y versus X3", ylab= "Number of people per thousand residng in urban areas in state", xlab= "per capital expenditure on shelters/housing assistance in state")
## plotting (x1,x2)
plot(expenditure$X1,expenditure$X2, main = "Scatter plot of X1 versus X2", ylab= "Number of residents per 100,000 that are financially insecure in state", xlab= "per capita personal income in state")
## plotting (x1,X3)
plot(expenditure$X1,expenditure$X3, main = "Scatter plot of X1 versus X3", ylab= "Number of people per thousand residing in urban areas in state", xlab= "per capita personal income in state")
## plotting (x3,x2)
plot(expenditure$X3,expenditure$X2, main = "Scatter plot of X3 versus X2", ylab= "Number of residents per 100,000 that are financially insecure in state", xlab= "Number of people per thousand residing in urban areas in state")

## Part 2
## plotting y and region
plot(expenditure$Region,expenditure$Y, main = "Histogram of Y and Region", xlab= "Region", ylab= "per capita expenditure on shelters/housing assistance in state")


## Part 3
## plotting y and X1
plot(expenditure$X1,expenditure$Y, main = "Scatter plot of Y versus X1", ylab= "per capita personal income in state", xlab= "per capital expenditure on shelters/housing assistance in state")
## adding in region - there are three variables now so I am going to use a 3d scatter plot 
scatterplot3d(expenditure$X1,expenditure$Region,expenditure$Y, main= "Plot of Y and X1 with Region", xlab= "per capital expenditure on shelters/housing assistance in state", ylab= "Region", zlab= "per capita personal income in state")
## setting different colors and shapes
different_regions <- c(1,2,3,4)
colors <- c("red", "green", "blue", "orange")
shapes <- c(17,18,15,16)
## setting default colors and shapes
point_colors <- rep("black", length(expenditure$Region))
point_shapes <- rep(20, length(expenditure$Region))
# making sure each region is assigned a certain shape and color
for (i in seq_along(different_regions)) {
  point_colors[expenditure$Region == different_regions[i]] <- colors[i]
  point_shapes[expenditure$Region == different_regions[i]] <- shapes[i]
}
## making the 3d scatterplot
scatterplot3d(expenditure$X1,expenditure$Region,expenditure$Y, 
              color= point_colors,
              pch= point_shapes,
              main= "Plot of Y and X1 with Region", 
              xlab= "per capital expenditure on shelters/housing assistance in state", 
              ylab= "Region", 
              zlab= "per capita personal income in state")




## making the a normal scatterplot just in case the first one is wrong
plot(expenditure$X1~expenditure$Y, 
              col= point_colors,
              pch= point_shapes,
              main= "Plot of Y and X1 with Region", 
              xlab= "per capital expenditure on shelters/housing assistance in state", 
              ylab= "per capita personal income in state")
legend("topleft",
       legend=c("Northeast", "North Central", "South", "West"),
                 col= point_colors,
                 pch=point_shapes)



