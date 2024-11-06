#####################
# Question 1: Political Science
#####################

#The following table was created using the data from a study run in a major Latin American city.1 
#As part of the experimental treatment in the study, one employee of the research team was chosen 
#to make illegal left turns across traffic to draw the attention of the police officers on shift. 
#Two employee drivers were upper class, two were lower class drivers, and the identity of the driver 
#was randomly assigned per encounter. The researchers were interested in whether officers were more 
#or less likely to solicit a bribe from drivers depending on their class (officers use phrases like, 
#“We can solve this the easy way” to draw a bribe). The table below shows the resulting data.

######### (a) Calculate the χ2 test statistic #########

# 1) building the observed frequency table
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
observed

# 2) calculating the sums for rows, columns and grand total
row_sums <- rowSums(observed)
col_sums <- colSums(observed)
grand_total <- sum(observed)

# 3) calculating the expected frequencies
expected <- outer(row_sums, col_sums) / grand_total
expected

# stargazer library to save table as .tex
install.packages("stargazer")
library(stargazer)
stargazer(expected, type = "text", out = "expected_table.tex")

# 4) calculating the x2 test statistic
chi_square_stat <- sum((observed - expected)^2 / expected)
chi_square_stat

# 5) calculating the degrees of freedom
df <- (nrow(observed) - 1) * (ncol(observed) - 1)
df


######### (b) Calculate the p-value. What do you conclude if α = 0.1? ######### 

# calculating the p-value
p_value <- pchisq(chi_square_stat, df = df, lower.tail = FALSE)
p_value

# Conclusion:
# Since the p-value (0.151) is greater than α = 0.1, it fails to reject the null hypothesis. 
# This means there is not enough evidence to conclude that the class of the driver 
# significantly affects whether a bribe is solicited or not.


######### (c) Calculate the standardized residuals for each cell and put them in the table #########


# observed and expected frequencies 
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
observed

expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
expected

# calculating the standardized residuals
standardized_residuals <- (observed - expected) / sqrt(expected)
standardized_residuals



######### (d) How might the standardized residuals help you interpret the results? #########


# The largest standardized residual is in the "Bribe Requested" cell for lower-class drivers, 
# with a value of 1.72. This suggests that lower-class drivers were more likely to be asked 
# for bribes than we would expect under the null hypothesis of no relationship between class and outcome.
# On the other hand, upper-class drivers seem to have experienced a lower rate of bribe requests 
# than expected. Thus, some deviation is visible for the "Stopped/Given Warning" category, but it's not
# as strong as the "Bribe Requested" result.







#####################
# Question 2: Economics
#####################


######### (a) State a null and alternative (two-tailed) hypothesis #########


# Null Hypothesis (H₀): 
# The reservation policy does NOT have an effect on the number of new or 
# repaired drinking water facilities in the villages. 

# Alternative Hypothesis (H₁): 
# The reservation policy DOES have an effect on the number of new or 
# repaired drinking water facilities. 


######### (b) Run a bivariate regression to test this hypothesis in R #########

getwd()
setwd("/Users/rafaelaalves/Documents/GitHub/StatsI_Fall2024/problemSets/PS02/my_answers_PS2")
getwd()

#uploading the dataset
data <- read.csv("dataset_ok.csv")
head(data)

# fitting a linear model
model <- lm(water ~ reserved, data = data)
summary(model)


######### (c) Interpret the coefficient estimate for reservation policy #########

# The variable "reserved" has a coefficient of 7.564, which means that villages with reserved 
# female leaders tend to have about 7.56 more drinking water facilities compared to those without
# reserved leaders. And considering the p-value (0.0136), we can reject the null-hypothesis 
# and consider more evidences for the alternative-hypothesis that reservations with female leaderships
# do have more drinking water facilities. 








