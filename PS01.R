#####################
# load libraries
# set wd
# clear global .envir
#####################

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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# First step, look at data
View(y)
head(y)
str(y) # Structure of R object
?str

mean(y)# Point estimate
sd(y)/sqrt(length(y))# Standard error



# 90% of observations lie within +/-1.645 
# standard errors of point estimate 

# The **approximate** solution 
# Lower bound, 90 confidence level
lower_90 = mean(y) - (1.645*sd(y)/sqrt(length(y)))


# Upper bound, 90 confidence level
upper_90 = mean(y) + (1.645*sd(y)/sqrt(length(y)))


# Print
lower_90
mean(y)
upper_90

# The **precise** solution, using normal distribution
# Lower bound, 90 confidence level
lower_90_n <- qnorm(0.05, 
                    mean = mean(y), 
                    sd = (sd(y)/sqrt(y)))

# Upper bound, 90 confidence level
upper_90_n <- qnorm(0.95,
                    mean = mean(y),
                    sd = (sd(y)/sqrt(length(y))))


# Print
lower_90_n
mean(y)
upper_90_n

cat("90% confidence interval for the average student IQ:", lower_90, "to", upper_90, "\n")
#Hypothesis Test``
t_test_result<-t.test(y, mu=100, alternative = "greater")
t_test_result

t_test_result$statistic
t_test_result$p.value
cat("Test Statistic:", t_test_result$statistic, "\n", "P-value:", t_test_result$p.value,"\n")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
str(expenditure)
pdf("Scatterplot matrix.pdf")
#Plot the relationships among Y, X1, X2, and X3
# Scatterplot matrix for Y, X1, X2, and X3
pairs(expenditure[, c("Y", "X1", "X2", "X3")], 
      main = "Scatterplot for Y, X1, X2, X3",
      pch = 16, 
      col = "blue")
dev.off()
#Calculate matrix of the correlations
cor(expenditure[, c("Y", "X1", "X2", "X3")])

#Plot the relationship between Y and Region
#Box plot
pdf("Boxplot.pdf")
boxplot(Y ~ Region, 
        data = expenditure, 
        main = "Per Capita Expenditure by Region", 
        xlab = "Region", 
        ylab = "Per Capita Expenditure",
        col=c("red", "green", "blue", "purple"))
# Add labels for regions
axis(1, at=1:4, labels = c("Northeast", "North Central", "South", "West"))
dev.off()
#On average, which region has the highest per capita expenditure on housing assistance?
# Calculate the mean expenditure for each region
tapply(expenditure$Y, expenditure$Region, mean)
cat("West region has the highest per capita expenditure on housing assistance")

#Plot the relationship between Y and X1
# Scatter plot for Y and X1
pdf("YX1.pdf")
plot(expenditure$X1, expenditure$Y, 
     main = "Relationship between Y and X1",
     xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Housing Assistance (Y)",
     pch = 16, col = "blue")
dev.off()
#Reproduce the above graph including one more variable Region and display different regions with different types of symbols and colors
# Scatter plot for Y and X1, colored by Region
pdf("YX1R.pdf")
plot(expenditure$X1, expenditure$Y, 
     main = "Y vs X1 by Region",
     xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Housing Assistance (Y)",
     pch = as.numeric(expenditure$Region), 
     col = c("red", "green", "blue", "purple")[expenditure$Region])

# Add a legend for regions
legend("topright",
       legend = c("Northeast", "North Central", "South", "West"), 
       col = c("red", "green", "blue", "purple"), 
       pch = 1:4,
       cex = 0.3)
dev.off()