#section 1
#question a
#Observed frequencies
observed <- matrix(c(14, 6, 7,  
                     7, 7, 1), 
                   nrow = 2, byrow = TRUE)

# Calculate total frequency (grand total)
grand_total <- sum(observed) 

# Calculate row totals and column totals
row_totals <- rowSums(observed)  
col_totals <- colSums(observed) 

# Calculate expected frequency for each cell
fe_11 <- (row_totals[1] / grand_total) * col_totals[1]  # Upper class, Not Stopped 
fe_12 <- (row_totals[1] / grand_total) * col_totals[2]  # Upper class,Bribe requested
fe_13 <- (row_totals[1] / grand_total) * col_totals[3]  # Upper class, Stopped/given warning

fe_21 <- (row_totals[2] / grand_total) * col_totals[1]  # Lower class, Not Stopped
fe_22 <- (row_totals[2] / grand_total) * col_totals[2]  # Lower class, Bribe requested
fe_23 <- (row_totals[2] / grand_total) * col_totals[3]  # Lower class, Stopped/given warning

# Put the calculated expected frequencies into a matrix
expected <- matrix(c(fe_11, fe_12, fe_13, 
                     fe_21, fe_22, fe_23), 
                   nrow = 2, byrow = TRUE)

# Calculate the X^2 test statistic using the manually calculated expected frequencies
chi_square_stat <- sum((observed - expected)^2 / expected)
chi_square_stat
#chi_square_stat should be approximately 3.7912



# question b 
# Chi-square statistic from part (a)
chi_square <- 3.7912

# Degrees of freedom
# (number of rows - 1) * (number of columns - 1)
df <- (2 - 1) * (3 - 1) 

# Calculate p-value
p_value <- pchisq(chi_square, df = df, lower.tail = FALSE)
p_value
# p_value should be approximately 1.1502282

# Set significance level
alpha <- 0.1

# Interpret the result
if (p_value < alpha) {
  cat("Reject the null hypothesis at α =", alpha, "\n")
} else {
  cat("Fail to reject the null hypothesis at α =", alpha, "\n")
}

#The p-value(1.1502282) is greater than alpha(0.1),so we cannot reject the null hypothesis,
#and there is no significant association between a driver's social class and the police officer's behavior.

#question c
# Perform chi-square test to get expected frequencies
chi_square_test <- chisq.test(observed)
expected <- chi_square_test$expected

# Calculate total frequency 
grand_total <- sum(observed)

# Calculate standardized residuals
std_residuals <- (observed - expected) / sqrt(expected*(1-rowSums(observed)/grand_total)*(1-colSums(observed)/grand_total))
round(std_residuals, 3)

# Create a data frame for easier viewing
residuals_df <- as.data.frame(std_residuals)
residuals_df$Class <- rownames(residuals_df)
residuals_df <- residuals_df[, c(4, 1, 2, 3)]

residuals_df
	
# question d




#section2
#question a 
	#Null Hypothesis (H0): There is no difference in the number of new or repaired drinking water facilities between villages with reserved and unreserved council heads.
#H0: μreserved = μunreserved
  #Alternative Hypothesis (Ha): There is a difference in the number of new or repaired drinking water facilities between villages with reserved and unreserved council heads.
#Ha: μreserved ≠ μunreserved

# question b


# Load the dataset
data_url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read.csv(data_url)

# View the first few rows of the dataset
head(data)

# Run bivariate regression
model <- lm(water ~ reserved, data = data)

# Display summary of the regression model
summary(model)

# p-value:0.0197 so we reject the null hypothesis

# question c

#The coefficient estimate Std.for the reservation policy is 9.252,meaning that 
#there is a positive association between the reservation policy and the number of new or repaired drinking water facilities in villages. 
#On average, villages that implemented the reservation policy had 9.252 more such facilities compared to those without the policy.

