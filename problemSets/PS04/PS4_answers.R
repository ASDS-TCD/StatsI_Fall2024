#####################
# load libraries
# set wd
# clear global .envir
#####################

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loading the necessary packages and data
# install.packages("car")
# install.packages("stargazer")
library(car)
library(stargazer)
data(Prestige)
help(Prestige)
summary(Prestige)

# Question 1
# Creating a new variable 
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Checking the results 
table(Prestige$professional)

# Running a regression model with interaction 
model1 <- lm(prestige ~ income + 
                        professional + 
                        income*professional, data = Prestige)
summary(model1)

# Creating a table of the results 
stargazer(model1, type = "latex", title = "Linear Regression Results", 
          out = "regression_table_model1.tex")


# Question 2
# Calculating the t-statistics for the coefficient beta_1 
beta_1 <- 0.042
se_1 <- 0.016
t_statistics_1 <- (beta_1 - 0) / se_1
print(round(t_statistics_1, 4))

# Calculate the p-value based on the t-statistics beta_1
n <- 131
k <- 2
p_value_1 <- 2 * pt(abs(t_statistics_1), df = n - k, lower.tail = FALSE)
print(round(p_value_1, 4))


# Calculating the t-statistics for the coefficient beta_2
beta_2 <- 0.042
se_2 <- 0.013
t_statistics_2 <- (beta_2 - 0) / se_2
print(round(t_statistics_2, 4))

# Calculate the p-value based on the t-statistics beta_2
n <- 131
k <- 2
p_value_2 <- 2 * pt(abs(t_statistics_2), df = n - k, lower.tail = FALSE)
print(round(p_value_2, 4))
