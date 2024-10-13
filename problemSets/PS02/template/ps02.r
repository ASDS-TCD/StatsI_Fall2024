#section 1
#question a
# Create the observed data matrix
observed <- matrix(c(14, 6, 7,
                     7, 7, 1),
                   nrow = 2,
                   byrow = TRUE)

# Calculate the total sum
N <- sum(observed)

# Calculate row and column totals
row_totals <- rowSums(observed)
col_totals <- colSums(observed)

# Calculate expected frequencies
expected <- outer(row_totals, col_totals) / N

# Calculate chi-square statistic manually
chi_square <- sum((observed - expected)^2 / expected)

# Print the manually calculated result
cat("Manually calculated Chi-square statistic:", chi_square, "\n")

# Verify using R's built-in chi-square test function
chi_square_test <- chisq.test(observed)
cat("R's chisq.test() result:", chi_square_test$statistic, "\n")

# Calculate degrees of freedom
df <- (nrow(observed) - 1) * (ncol(observed) - 1)
cat("Degrees of freedom:", df, "\n")

# Calculate p-value
p_value <- pchisq(chi_square, df = df, lower.tail = FALSE)
cat("p-value:", p_value, "\n")

# Set significance level
alpha <- 0.1

# Interpret the result
if (p_value < alpha) {
  cat("Reject the null hypothesis at α =", alpha, "\n")
} else {
  cat("Fail to reject the null hypothesis at α =", alpha, "\n")
}

#question b 
# Chi-square statistic from part (a)
chi_square <- 3.7805

# Degrees of freedom
# (number of rows - 1) * (number of columns - 1)
df <- (2 - 1) * (3 - 1) 

# Calculate p-value
p_value <- pchisq(chi_square, df = df, lower.tail = FALSE)

# Print p-value
cat("p-value:", p_value, "\n")

# Set significance level
alpha <- 0.1

# Interpret the result
if (p_value < alpha) {
  cat("Reject the null hypothesis at α =", alpha, "\n")
} else {
  cat("Fail to reject the null hypothesis at α =", alpha, "\n")
}


#question c
# Create the observed data matrix
observed <- matrix(c(14, 6, 7,7, 7, 1),nrow = 2, byrow = TRUE,
                   dimnames = list(c("Upper class", "Lower class"),
                                   c("Not Stopped", "Bribe requested", "Stopped/given warning")))

# Perform chi-square test to get expected frequencies
chi_square_test <- chisq.test(observed)
expected <- chi_square_test$expected

# Calculate standardized residuals
std_residuals <- (observed - expected) / sqrt(expected)

# Print the standardized residuals
print(round(std_residuals, 3))

# Create a data frame for easier viewing
residuals_df <- as.data.frame(std_residuals)
residuals_df$Class <- rownames(residuals_df)
residuals_df <- residuals_df[, c(4, 1, 2, 3)]

# Print the data frame
print(residuals_df)
	
	
	
	
# question d
# Create the observed data matrix
observed <- matrix(c(14, 6, 7,
                  7, 7, 1),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("Upper class", "Lower class"),
                                   c("Not Stopped", "Bribe requested", "Stopped/given warning")))

# Perform chi-square test to get expected frequencies
chi_square_test <- chisq.test(observed)
expected <- chi_square_test$expected

# Calculate standardized residuals
std_residuals <- (observed - expected) / sqrt(expected)

# Print the standardized residuals
print(round(std_residuals, 3))

# Create a data frame for easier viewing
residuals_df <- as.data.frame(std_residuals)
residuals_df$Class <- rownames(residuals_df)
residuals_df <- residuals_df[, c(4, 1, 2, 3)]

# Print the data frame
print(residuals_df)


#section2
#question a
	#Null Hypothesis (H0): There is no difference in the number of new or repaired drinking water facilities between villages with reserved and unreserved council heads.
#H0: μreserved = μunreserved
  #Alternative Hypothesis (Ha): There is a difference in the number of new or repaired drinking water facilities between villages with reserved and unreserved council heads.
#Ha: μreserved ≠ μunreserved

# question b
	# Load necessary library
library(tidyverse)

# Load the dataset
data_url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read.csv(data_url)

# View the first few rows of the dataset
head(data)

# Run bivariate regression
model <- lm(new_or_repaired ~ reservation, data = data)

# Display summary of the regression model
summary(model)

# question c

lm(formula = new_or_repaired ~ reservation, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
...

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)   ...       ...        ...     ...
reservation   ...       ...        ...     ...


