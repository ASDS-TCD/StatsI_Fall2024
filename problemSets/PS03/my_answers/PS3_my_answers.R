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
setwd(Users/rafaelaalves/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers)
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
pdf("plot_question1.pdf")
plot(inc.sub$difflog, inc.sub$voteshare,
     xlab = "difflog", 
     ylab = "voteshare", 
     main = "voteshare vs. difflog",
)
abline(model1, col = "red", lwd = 3)
dev.off()

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
pdf("plot_question2.pdf")
plot(inc.sub$difflog, inc.sub$presvote,
     xlab = "difflog", 
     ylab = "presvote", 
     main = "presvote vs. difflog",
)
abline(model2, col = "blue", lwd = 3)
dev.off()

# 3. Save the residuals of the model in a separate object
residuals_model2 <- residuals(model2)
head(residuals_model2)


# 4. Write the prediction equation.
coefficients(model2)
# presvote = 0.507 + 0.023 * difflog
# For every one-unit increase in difflog, the predicted presvote increases by approximately 0.023 units.
# The starting point for presvote when difflog is zero is around 0.507.



# ____________________________________________QUESTION 3___________________________________________
# _________________________________________________________________________________________________


# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.
model3 <- lm(voteshare ~ presvote, data=inc.sub)
summary(model3)


# 2. Make a scatterplot of the two variables and add the regression line.
pdf("plot_question3.pdf")
plot(inc.sub$presvote, inc.sub$voteshare,
     xlab = "presvote", 
     ylab = "voteshare", 
     main = "voteshare vs. presvote",
)
abline(model3, col = "green", lwd = 3)
dev.off()

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
pdf("plot_question4.pdf")
plot(residuals_model2, residuals_model1,
     xlab = "residuals 2", 
     ylab = "residuals 1", 
     main = "residuals_model1 vs. residuals_model2",
)
abline(reg_residuals, col = "orange", lwd = 3)
dev.off()

# 3. Write the prediction equation.
coefficients(reg_residuals)
# residuals_model1 = -5.93 + 2.56 * residuals_model2
# For each unit increase in the residuals_model2, the residuals_model1 increase by approximately 2.56 units. 
# The starting point for residuals_model1 is negative, -5.93, when residuals_model2 is zero.




# ____________________________________________QUESTION 5___________________________________________
# _________________________________________________________________________________________________

# 1. Run a regression where the outcome variable is the incumbent’s voteshare 
# and the explanatory variables are difflog and presvote.

# Y = voteshare
# X1 = difflog (campaing spending)
# X2 = presvote (president’s popularity)

model5 <- lm(voteshare ~ difflog + presvote, data=inc.sub)
summary(model5)


# 2. Write the prediction equation.
coefficients(model5)
# voteshare = 0.448 + 0.035 * difflog + 0.256 * presvote
# For each unit increase in difflog, the voteshare increases by approximately 0.035 units, assuming presvote remains constant. 
# For each unit increase in presvote, the voteshare increases by approximately 0.256 units, assuming difflog remains constant. 
# The starting point for voteshare is 0.448 when both, difflog and presvote are zero.


# 3. What is it in this output that is identical to the output in Question 4? Why do you think this is the case?


# The RESIDUALS in Question 4 are identical to the residuals in Question5. 
# This can be explained by how the relationships between predictors are involved.

# In the regression in Question 4, we're regressing the residuals from model1 (what remains in voteshare after accounting for difflog) 
# and model2 (what remains in presvote after accounting for difflog)
# It checks if there is a relationship between these two residuals, which corresponds to the part of voteshare explained by presvote after controlling for difflog.

# And in Question 5, In REGRESSION 5, we use difflog and presvote as predictors of voteshare. 
# The residuals in this case are what remains in voteshare after accounting difflog and presvote. 

# Summarizing, both sets of residuals are showing the "unexplained" part of voteshare after accounting the effect of difflog and presvote.
