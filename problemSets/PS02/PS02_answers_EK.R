setwd("/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS02/my answers")
getwd()

# PROBLEM SET 2
# QUESTION 1

# Inputting the observed data
observed_table <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Assign row and column labels 
rownames(observed_table) <- c("Upper class", "Lower class")
colnames(observed_table) <- c("Not stopped", "Bribe requested", "Stopped/given warning")

# Convert to a table with observed frequencies 
observed_table <- as.table(observed_table)
observed_table

# Calculate the sums 
row_totals <- rowSums(observed_table)
column_totals <- colSums(observed_table)
grand_total <- sum(observed_table)
row_totals 
column_totals 
grand_total 

# Generate the table with expected frequencies 
expected_table <- outer(row_totals, column_totals) / grand_total
expected_table

# Calculating the Chi-sqr statistic
chi_sqr_table <- (observed_table - expected_table)^2 / expected_table
chi_sqr_table
chi_sqr <- sum(chi_sqr_table)
chi_sqr

#Calculating the p-value and degrees of freedom 
p_value <- pchisq(chi_sqr, df = df, lower.tail = FALSE)
round(p_value, 2)
df <- (nrow(observed_table) -1) * (ncol(observed_table) - 1)
df

#Checking the results using the Chi-sqr function 
# Warning - one column has freq less han 5 

chi_sqr <- chisq.test(observed_table)
chi_sqr

# Calculating Standardized residuals 
prop_rows <- prop.table(observed_table, margin = 1)
prop_cols <- prop.table(observed_table, margin = 2)
prop_rows
prop_cols

residuals <- (observed_table - expected_table) / sqrt(expected_table * (1 - prop_rows) * (1 - prop_cols))
round(residuals, 2)

# QUESTION 2

#Loading the data and exploring 
df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
summary(df)
table(df$reserved)
table(df$female)
mean(df$water)
tapply(df$water, df$female, mean)
tapply(df$water, df$reserved, mean)
female_reserved_table <- table(df$female, df$reserved)
female_reserved_table

# Labeling the data for exploring 
#df$female <- factor(df$female, labels = c("Male", "Female"))
#df$reserved <- factor(df$reserved, labels = c("Not reserved", "Reserved"))str(df)

# Linear regression model
lm(df$water~df$reserved)

# Saving the model as an object
model <- lm(df$water~df$reserved)

# Creating a table of the results 
install.packages("stargazer")
library(stargazer)
stargazer(model, type = "latex", title = "Table: Linear Regression Results", out = "regression_table.tex")





