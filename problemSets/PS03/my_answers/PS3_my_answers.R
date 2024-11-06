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

# set wd for current folder
setwd(Users/rafaelaalves/Documents/GitHub/StatsI_Fall2024/problemSets/PS03)
getwd()

# read in data
inc.sub <- read.csv("/Users/rafaelaalves/Documents/GitHub/StatsI_Fall2024/datasets/incumbents_subset.csv")
head(inc.sub)
View(inc.sub)
class(inc.sub)


# ____________________________________________QUESTION 1___________________________________________
# _________________________________________________________________________________________________

# 1-Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.
model1 <- lm(voteshare ~ difflog, data=inc.sub)
summary(model1)


# 2. Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "difflog", 
     ylab = "voteshare", 
     main = "Scatterplot of Vote Share vs. Difflog",
)
abline(model1, col = "red", lwd = 3)



# 3. Save the residuals of the model in a separate object.
residuals_model1 <- residuals(model1)
head(residuals_model1)


# 4. Write the prediction equation.
coefficients(model1)
# voteshare = 0.57 + 0.04 * difflog
# For every one-unit increase in difflog, the predicted voteshare increases by approximately 0.0417 units.
# The starting point for voteshare when difflog is zero is around 0.579.





# ____________________________________________QUESTION 2___________________________________________
# _________________________________________________________________________________________________


# 1. Run a regression where the outcome variable is presvote and the explanatory variable is difflog.
model2 <- lm(presvote ~ difflog, data=inc.sub)
summary(model2)


# 2. Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "difflog", 
     ylab = "presvote", 
     main = "Scatterplot of presvote vs. difflog",
)
abline(model2, col = "blue", lwd = 3)


# 3. Save the residuals of the model in a separate object
residuals_model2 <- residuals(model2)
head(residuals_model2)


# 4. Write the prediction equation.
coefficients(model2)
# presvote = 0.50 + 0.023 * difflog
# For every one-unit increase in difflog, the predicted presvote increases by approximately 0.023 units.
# The starting point for presvote when difflog is zero is around 0.507.



# ____________________________________________QUESTION 3___________________________________________
# _________________________________________________________________________________________________


# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.
model3 <- lm(voteshare ~ presvote, data=inc.sub)
summary(model3)


# 2. Make a scatterplot of the two variables and add the regression line.
plot(inc.sub$presvote, inc.sub$voteshare,
     xlab = "presvote", 
     ylab = "voteshare", 
     main = "Scatterplot of voteshare vs. presvote",
)
abline(model3, col = "green", lwd = 3)


# 3. Write the prediction equation.
coefficients(model3)
# voteshare = 0.44 + 0.388 * presvote
# For every one-unit increase in presvote, the predicted voteshare increases by approximately 0.388 units.
# The starting point for voteshare when presvote is zero is around 0.44.



# ____________________________________________QUESTION 4___________________________________________
# _________________________________________________________________________________________________

# 1. Run a regression where the outcome variable is the residuals from Question 1 
# and the explanatory variable is the residuals from Question 2.

# Y outcome = residuals_model1
# X predictor = residuals_model2

reg_residuals <- lm(residuals_model1 ~ residuals_model2)
summary(reg_residuals)


# 2. Make a scatterplot of the two residuals and add the regression line.

plot(residuals_model2, residuals_model1,
     xlab = "residuals 2", 
     ylab = "residuals 1", 
     main = "Scatterplot of residuals",
)
abline(reg_residuals, col = "orange", lwd = 3)


# 3. Write the prediction equation.
coefficients(reg_residuals)
# residuals_model1 = -5.93 + 2.56 * residuals_model2
# For each unit increase in the residuals_model2, the residuals_model1 increase by approximately 2.56 units. 
# The starting point for residuals_model1 is negative, -5.93, when residuals_model2 is zero.





