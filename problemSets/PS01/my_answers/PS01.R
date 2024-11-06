#####################
# Problem 1
#####################


#A school counselor was curious about the average of IQ of the students in her school and
#took a random sample of 25 students’ IQ scores. The following is the data set:
# y <− c ( 1 0 5 , 6 9 , 8 6 , 1 0 0 , 8 2 , 1 1 1 , 1 0 4 , 1 1 0 , 8 7 , 1 0 8 , 8 7 , 
#9 0 , 9 4 , 1 1 3 , 1 1 2 , 9 8 , 8 0 , 9 7 , 9 5 , 1 1 1 , 1 1 4 , 8 9 , 9 5 , 1 2 6 , 9 8 )


# 1. Find a 90% confidence interval for the average student IQ in the school.

# data set:
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)

# exploring the data set
mean_y <- mean(y)
cat("Mean:", mean_y, "\n")
sd_y <- sd(y)
cat("Standard Deviation:", sd_y, "\n")
class(y)
length(y)

# 90% confidence interval using t.test
confidence_interval <- t.test(y, conf.level = 0.90)
confidence_interval$conf.int


# 2. Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.
#Using the same sample, conduct the appropriate hypothesis test with α = 0.05.

# data set:
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98,
       80, 97, 95, 111, 114, 89, 95, 126, 98)

# one-sample t-test:
t_test_result <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.05)
t_test_result

# Conclusion:
# Because the p-value is 0.7215 and it's greater than 0.05, I do not have enough evidence 
# to reject the null hypothesis that average student IQ in the school is equal to the 
# national average of 100 IQ score.


#####################
# Problem 2
#####################


# uploading expenditure data set
getwd()
setwd("/Users/rafaelaalves/Documents/GitHub/StatsI_Fall2024/problemSets/PS01/my_answers")


# reading the file
expenditure <- read.table("expenditure.txt", header = TRUE, sep = "\t")
head(expenditure)
str(expenditure)
View(expenditure)


# installing stargazer package to save table view as a .tex
install.packages("stargazer")
library(stargazer)
stargazer(expenditure, type = "text", out = "expenditure_table.tex")


# Scatter plot of Y vs X1
plot(expenditure$X1, expenditure$Y, main="Y vs X1", xlab="personal income", ylab="expenditure on shelters", col="blue", pch=19)
pdf("plot_example1.pdf")
plot(expenditure$X1, expenditure$Y,
     main="Y vs X1", 
     xlab="personal income", 
     ylab="expenditure on shelters", 
     col="blue", 
     pch=19)
dev.off()
# There's a correlation between the per capita expenditure in shelters related to the group that has a personal income of 1.600 to 2.100 per capita
# which doesn't make sense to me because I think the state should spend more on housing assistance with people with lower personal income


# Scatter plot of Y vs X2
plot(expenditure$X2, expenditure$Y, main="Y vs X2", xlab="financially insecure residents", ylab="expenditure on shelters", col="red", pch=19)
pdf("plot_example2.pdf")
plot(expenditure$X2, expenditure$Y,
     main="Y vs X2", 
     xlab="financially insecure residents", 
     ylab="expenditure on shelters", 
     col="red", 
     pch=19)
dev.off()
# the spent per capita for housing assistance is higher where there are more financially insecure residents which makes sense to me because 
# the housing assistance spending are concentrated where the demographic density are higher for financially insecure residents


# Scatter plot of Y vs X3
plot(expenditure$X3, expenditure$Y, main="Y vs X3", xlab="residing in urban areas", ylab="expenditure on shelters", col="green", pch=19)
pdf("plot_example3.pdf")
plot(expenditure$X3, expenditure$Y,
     main="Y vs X3", 
     xlab="residing in urban areas", 
     ylab="expenditure on shelters", 
     col="green", 
     pch=19)
dev.off()
# the graph shows that spending in housing assistance is lower in rural areas and hight in urban areas


# Scatter plot of X1 vs X2
plot(expenditure$X1, expenditure$X2, main="X1 vs X2", xlab="personal income", ylab="financially insecure residents", col="orange", pch=19)
pdf("plot_example4.pdf")
plot(expenditure$X1, expenditure$X2,
     main="X1 vs X2", 
     xlab="personal income", 
     ylab="financially insecure residents", 
     col="orange", 
     pch=19)
dev.off()
# the residents with personal income around 1.500 to 2.100 are the ones with a higher financially insecure situation


# Scatter plot of X1 vs X3
plot(expenditure$X1, expenditure$X3, main="X1 vs X3", xlab="personal income", ylab="residing in urban areas", col="purple", pch=19)
pdf("plot_example5.pdf")
plot(expenditure$X1, expenditure$X3,
     main="X1 vs X3", 
     xlab="personal income", 
     ylab="residing in urban areas", 
     col="purple", 
     pch=19)
dev.off()
# people with lower personal income reside un rural areas, and higher incomes in urban areas


# Scatter plot of X2 vs X3
plot(expenditure$X2, expenditure$X3, main="X2 vs X3", xlab="financially insecure residents", ylab="residing in urban areass", col="grey", pch=19)
pdf("plot_example6.pdf")
plot(expenditure$X2, expenditure$X3,
     main="X2 vs X3", 
     xlab="financially insecure residents", 
     ylab="residing in urban areas", 
     col="grey", 
     pch=19)
dev.off()
# even though people with lower personal incomes live in rural areas, there are less people financially insecure living in rural areas 
# (can we say that personal income is not related with financial insecure?? income is lower in rural areas but cost of life is cheaper?)


# Scatter plot of Y vs Region
plot(expenditure$Region, expenditure$Y, main="Y vs Region", xlab="1 = NE, 2 = NC, 3 = S, 4 = W", ylab="expenditure on shelters", col="pink", pch=19)
pdf("plot_example7.pdf")
plot(expenditure$Region, expenditure$Y,
     main="Y vs Region", 
     xlab="1 = North East, 2 = North Central, 3 = South, 4 = West", 
     ylab="expenditure on shelters", 
     col="pink", 
     pch=19)
dev.off()
# On average, the West region has the highest per capita expenditure on housing assistance.


############## Reproducing Y vs X1 including one more variable Region and
############## displaying different regions with different types of symbols and colors.
install.packages("ggplot2")
library(ggplot2)

# Sample data
expenditure <- data.frame(
  Y = rnorm(100, mean = 50, sd = 10),  # Random data for Y
  X1 = rnorm(100, mean = 30, sd = 5),  # Random data for X1
  Region = sample(c("1.Northeast", "2.North Central", "3.South", "4.West"), 100, replace = TRUE)  # Random regions
)

# printing a PDF
pdf("plot_example8.pdf", width = 7, height = 5)  # Adjust width and height if necessary

ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 3) +  # Adjust the size of the points if needed
  labs(title = "Y vs X1 vs Region",
       x = "personal income per capita",
       y = "expenditure on shelters") +
  scale_shape_manual(values = c(16, 17, 15, 3)) +  # Specify shapes: 16 = circle, 17 = triangle, 15 = square, 3 = plus
  theme_minimal()

dev.off()





