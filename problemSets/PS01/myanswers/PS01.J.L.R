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

# Section1:Calculate the 90% Confidence Interval

# Given the sample size is less than 30(n=25),so we can use t-test to calculate the confidence interval
# Sample size
n <- length(y)

# Set the significance level and get the t-critical value
alpha <- 0.10
t_critical <- qt(0.05, df=n-1)

# Calculate the standard deviation of the sample
sample_sd <- sd(y)

# This value reflects the uncertainty around the sample mean
margin_of_error <- t_critical * (sd(y) / sqrt(n))

#Calculate the confidence interval limits
CI_lower <- mean(y) - margin_of_error
CI_upper <- mean(y) + margin_of_error

#print confidence interval
print(c(CI_lower, mean(y), CI_upper)) 

# The calculated 90% confidence interval is approximately [93.95, 102.92] 

#####################
# Section2:Conduct the Hypothesis Test

#Perform a one-sample t-test to test whether the hypothesized mean is significantly greater than 100
# The null hypothesis (H0) is that the true mean IQ is equal to 100.
# The alternative hypothesis (H1) is that the true mean IQ is greater than 100.

#Perform the t-test
t_test_result <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

#Print the result of the t-test
print(t_test_result)

# The output includes:
# t-value: -0.59574
# p-value: 0.7215, which is quite large and suggests we fail to reject the null hypothesis.
# Alternative hypothesis: true mean is greater than 100.
# 95% Confidence Interval: [93.96, Inf] - The lower bound is 93.96, with no upper bound (as it's a one-tailed test).
# Sample mean: 98.44, which is less than 100
# aligning with the test's result that the average IQ is not significantly greater than 100.

#####################
# Problem 2
#####################

# Load required libraries
library(ggplot2)
library(dplyr)
library(corrplot)

# Import the data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header = TRUE)
head(expenditure)

# Rename columns for clarity
colnames(expenditure) <- c("State", "Y", "X1", "X2", "X3", "Region")

# Convert Region to factor
expenditure$Region <- factor(expenditure$Region,levels = 1:4,labels = c("Northeast", "North Central", "South", "West"))

# Create correlation matrix
cor_matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")])
print(cor_matrix)

# section 1:Plot relationships among Y, X1, X2, and X3
# Create scatter plots for all bivariate relationships
pdf(file="scatter_Matrix.pdf",width=8,height=6)

# Plot the pairwise scatterplots between Y, X1, X2, X3
pairs(expenditure[, c("Y", "X1", "X2", "X3")], pch = 19, col = alpha("black", 0.5))
dev.off()

# The correlation among Y, X1, X2, and X3
# Y and X1 show a moderate positive correlation (~0.53), suggesting that states with higher personal income tend to spend more on housing assistance.
# Y and X2 have a moderate positive correlation (~0.45), indicating a link between the number of financially insecure residents and housing expenditure.
# Y and X3 also show a moderate positive correlation (~0.46), implying that urbanization (X3) has some relationship with higher housing assistance spending.
# The relationships between X1 and X2, X1 and X3, and X2 and X3 show varying degrees of correlation.

#####################
#section 2: Plot the relationship between Y and Region
Y_vs_Region <- ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_point() +
  labs(title = "Per Capita Expenditure on Housing Assistance by Region",
       x = "Region", y = "Per Capita Expenditure")
ggsave("Y_VS_Region.pdf",plot=Y_vs_Region,width=8,height=6,units="in")

# Based on the plot, Region 1 (Northeast) has the highest average per capita expenditure on housing assistance.

#####################
# Section3:Plot the relationship between Y and X1
Y_vs_X1 <- ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point() +
  labs(title = "Per Capita Expenditure vs Per Capita Personal Income",
       x = "Per Capita Personal Income", y = "Per Capita Expenditure")
ggsave("Y_vs_X1.pdf",plot=Y_vs_X1,width=8,height=6,units="in")

# The scatterplot shows a positive linear relationship between Y and X1 (per capita income).
# Higher personal income in a state tends to be associated with higher spending on housing assistance.
 
# Reproduce the Y vs X1 scatterplot, adding Region as a distinguishing factor
Y_X1_Region <- ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 3) +
  labs(title = "Per Capita Expenditure vs Per Capita Personal Income by Region",
       x = "Per Capita Personal Income", y = "Per Capita Expenditure")
ggsave("Y_X1_Region.pdf",plot=Y_X1_Region,width=8,height=6,units="in")

# The plot shows that different regions have different expenditure trends with respect to personal income.
# Region 1 (Northeast) tends to have higher spending across the income spectrum, while Region 3 (South) generally shows lower expenditure.

# Additional bivariate plots as requested
X2_Y <- ggplot(expenditure, aes(x = X2, y = Y)) +
  geom_point() +
  labs(title = "Per Capita Expenditure vs Financially Insecure Residents",
       x = "Financially Insecure Residents per 100,000", y = "Per Capita Expenditure")
ggsave("X2_Y.pdf",plot=X2_Y,width=8,height=6,units="in")

X3_Y <- ggplot(expenditure, aes(x = X3, y = Y)) +
  geom_point() +
  labs(title = "Per Capita Expenditure vs Urban Population",
       x = "Urban Residents per 1,000", y = "Per Capita Expenditure")
ggsave("X3_Y.pdf",plot=X3_Y,width=8,height=6,units="in")

X1_X2 <- ggplot(expenditure, aes(x = X1, y = X2)) +
  geom_point() +
  labs(title = "Financially Insecure Residents vs Per Capita Personal Income",
       x = "Per Capita Personal Income", y = "Financially Insecure Residents per 100,000")
ggsave("X1_X2.pdf",plot=X1_X2,width=8,height=6,units="in")

X1_X3 <- ggplot(expenditure, aes(x = X1, y = X3)) +
  geom_point() +
  labs(title = "Urban Population vs Per Capita Personal Income",
       x = "Per Capita Personal Income", y = "Urban Residents per 1,000")
ggsave("X1_X3.pdf",plot=X1_X3,width=8,height=6,units="in")

X2_X3=ggplot(expenditure, aes(x = X2, y = X3)) +
  geom_point() +
  labs(title = "Urban Population vs Financially Insecure Residents",
       x = "Financially Insecure Residents per 100,000", y = "Urban Residents per 1,000")
ggsave("X2_X3.pdf",plot=X2_X3,width=8,height=6,units="in")