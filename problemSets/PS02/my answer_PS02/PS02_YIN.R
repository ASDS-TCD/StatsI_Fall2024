#####################
# Problem 1
#####################

#(a)
# Create the observed contingency table
observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Label the rows and columns
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")

# Print the observed data
print(observed)

# Row and column totals
row_totals <- rowSums(observed)
col_totals <- colSums(observed)
grand_total <- sum(observed)

row_totals
col_totals
grand_total
# Expected frequencies
expected <- outer(row_totals, col_totals) / grand_total
expected
# Chi-square test statistic
chi_square_stat <- sum((observed - expected)^2 / expected)

# Print the results of the test
chi_square_test


#(b)
# Degrees of freedom = (number of rows - 1) * (number of columns - 1)
df <- (2 - 1) * (3 - 1)

# Chi-square test statistic
chi_square_stat <- 3.7912

# Calculate p-value
p_value <- pchisq(chi_square_stat, df, lower.tail = FALSE)
p_value


#####################
# Problem 2
#####################
#(b)
# Load necessary libraries
library(tidyverse)

# Load the dataset
url <- "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read.csv(url)

# View the first few rows of the data
head(data)

# Run a bivariate regression
# Outcome variable: water 
# Predictor variable: reserved
model <- lm(water ~ reserved, data = data)

# Print the summary of the regression results
summary(model)
