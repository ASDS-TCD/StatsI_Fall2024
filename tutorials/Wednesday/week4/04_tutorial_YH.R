# Applied Statistical Analysis I/Quantitative Methods I      
# Tutorial 4: Bivariate regression, inference & prediction                     

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only = TRUE)
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

lapply(c("tidyverse", "stargazer"),  pkgTest)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

#############################
### RECAP Chi-square test ###
#############################

# Research questions: Is there a relationship between
# movie genre and rating?

# Load data
dm <- readRDS("movies_updated.rds")
View(dm)

# Dataframe subsetting: dm[rows, columns]
dm_s <- dm[dm$genre=="Comedy" |
             dm$genre=="Drama"  |
             dm$genre=="Documentary", ]
dm_s$genre <- droplevels(dm_s$genre)
View(dm_s)

# Run Chi squared test
chisq.test(dm_s$genre, 
           dm_s$critics_rating)

# Check p-value
sprintf("%.20f",1.097e-12)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

### Look at standardized residuals ###

# Save chi-square test in object
chi_test <- chisq.test(dm_s$genre, dm_s$critics_rating)

# List objects inside chi_test
ls(chi_test)
str(chi_test)
chi_test$observed # f_o (observed frequencies)
chi_test$expected # f_e (expected frequencies under the assumption of H0,
# under the assumption that two variables are independent)

# Pearson residuals, 
# (observed - expected) / sqrt(expected)
chi_test$residuals 

sum(dm_s$genre[dm_s$critics_rating == "Rotten"] == "Comedy")  #63

(63-37.22973)/sqrt(37.22973)


# **Standardized** residuals,
# (observed - expected) / sqrt(V), where V is the residual cell variance
chi_test$stdres  

addmargins(table(dm_s$genre, dm_s$critics_rating))

(63-37.22973)/sqrt(37.22973*(1-87/444)*(1-190/444)) #6.227389

# How can we interpret the standardized residuals? 

# Agenda 
# (a.) Correlation
# (b.) Bivariate regression 

# Research questions: 
# Is there a relationship between education and income?

# (a.) Correlation -----

# Load data 
df <- read.csv("fictional_data.csv")
View(df)

# Scatter plot 
plot(df$edu, df$income)

# Calculate correlation
cor(df$edu, df$income)

# Add to scatter plot
text(1.5, 3000, sprintf("Correlation=%s", round(cor(df$edu, df$income),4)))

# Improve visualization and save
png(file="scatter_plot.png")
plot(df$edu,
     df$income,
     xlab="University level education (in years)",
     ylab="Monthly net income (in Euro)",
     main="The Relationship between education and income") 
text(1.5, 3000, sprintf("Correlation=%s", round(cor(df$edu, df$income),4)))
dev.off()

# t-test for the correlation coefficient
cor.test(df$edu, df$income)

# Check p-value
sprintf("%.20f",7.52e-07)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# (b.) Bivariate regression  -----

# Fit linear regression model
summary(lm(df$income ~ df$edu))
summary(lm(income ~ edu, data = df))

# Save model as object
model <- lm(income ~ edu,  data = df)

# t-test for the slope of a regression line
summary(model)

# t value for edu
250.64/33.06 

# Check p-value
sprintf("%.20f", 2.17e-06)

# Step 1: Assumptions
# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# Confidence intervals 
confint(model, level = 0.95)
confint(model, level = 0.99)

# Plot
plot(x=df$edu, y=df$income) # Scatter plot
abline(model) # Add regression line

# Step by step
plot(x=df$edu, y=df$income) # Scatter plot
abline(v=4)  # Either specify single value (v for vertical)
abline(976.16, 250.64) # Or intercept and slope
abline(model) # Use intercept and slope in model object
abline(model, col="red") # Change color

# What is the prediction equation?
summary(model)
# income_pred = 976.16 + 250.64 * education

# Make predictions for first observation in df
head(df)
976.16 +  250.64 * 1 # predicted outcome

model$fitted.values

1520 - (976.16 +  250.64 * 1) # error


model$residuals


# Make predictions for a range of x values
predict(model, newdata = data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))

# Step by step
predict(model) # Predicted outcomes
model$fitted.values # Predicted outcomes
sort(unique(df$edu)) # Unique values of x
seq(min(df$edu), max(df$edu), by=1) # Specify a sequences for which
# predictions are to be returned
newdata = data.frame(edu = seq(min(df$edu), max(df$edu), by=1))
newdata

predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu), by=1)))
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)))

# Add standard errors
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), se.fit=TRUE)

# Make predictions with **confidence intervals**
# Predict an average response at any chosen value of x
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="confidence", level=0.95)

# Make predictions with **prediction intervals**
# Predict an individual’s response at any chosen value of x 
predict(model, newdata=data.frame(edu = c(0,1,2,3,4,5,6,7,8)), interval="prediction", level=0.95)
# more variability in individual responses --> wider intervals

# Make predictions for x values not in data
predict(model, newdata=data.frame(edu = mean(df$edu))) # Mean education
mean(df$edu)
sort(unique(df$edu)) # Unique values of x
predict(model, newdata=data.frame(edu = 9)) # **But don't extrapolate**

# Plot predictions
plot(x=df$edu, y=df$income) # Scatter plot
points(df$edu, model$fitted.values, # Add another scatter plot on top
       col="green")

# Plot, regression line with confidence intervals
# Adopted from: https://stackoverflow.com/questions/46459620/plotting-a-95-confidence-interval-for-a-lm-object

# Save confidence intervals
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
plot(df$edu, df$income) # Scatter plot
abline(model) # Add regression line
# Add lower bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[, 2], col="gray")
# Add upper bound
lines(seq(min(df$edu), max(df$edu),by=1), ci[, 3], col="gray")

# Step by step
ci <- predict(model, newdata=data.frame(edu = seq(min(df$edu), max(df$edu),by=1)), interval="confidence", level=0.95)
ci # Save confidence intervals in object
# Dataframe subsetting: df[rows, columns]
ci[, 2] # second column, lower bound, lwr
ci[, 3] # third column, upper bound, upr

# Improve visualization and save
png(file = "reg_plot.png")
plot(df$edu,
     df$income,
     xlab = "University level education (in years)",
     ylab = "Monthly net income (in Euro)",
     main = "The Relationship between education and income")
abline(model) # Add regression line
# Add confidence intervals
lines(seq(min(df$edu), max(df$edu),by=1), ci[,2], col="gray")
lines(seq(min(df$edu), max(df$edu),by=1), ci[,3], col="gray")
# Add legend
legend(0, 3100, # x and y position of legend
       legend = c("Predictions", "95% Confidence intervals"),
       col = c("black","gray"),
       pch = 1,
       cex = 0.6) 
dev.off()