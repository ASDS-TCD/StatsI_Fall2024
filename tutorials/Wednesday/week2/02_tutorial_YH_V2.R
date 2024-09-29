# Applied Statistical Analysis I      
# Tutorial 2: Hypothesis testing, experiments, difference in means                       

# Get working directory
getwd()

# Set working directory 
setwd("D:/TCD_courses/2024_2025Autumn/Applied StatsQuant Methods I_Fall 2024/tutorials/02")
getwd()

# Agenda
# (a.) Descriptive analysis
# (b.) Confidence intervals
# (c.) Significance test for a mean
# (d.) Significance test for a difference in means

### Research Question -----------
# Is there a relationship between education and income?

# Load data 
df <- read.csv("fictional_data.csv")

# Selection of variables
# Education: University level education in years
# Income: Monthly net income
# Capital: Whether the person lives in capital or not

### (a.) Descriptive analysis ----------

# First step, look at data
View(df)
head(df)
str(df) # Structure of R object
?str

mean(df$income) # Central tendency, mean
var(df$income) # Variability, variance
sd(df$income) # Variability, standard deviation
sd(df$income)/sqrt(length(df$income)) # Variability, standard **error**

# Step by step
df$income # Access variable
length(df$income) # Number of observations
sqrt(length(df$income)) # Take square root
sqrt(19)

# Get summary statistics for entire dataset
summary(df)

# Some quick visualizations, to look at distribution
hist(df$income,
     #breaks = 20,
     main = "Monthly net income",
     xlab = "Euro")


plot(density(df$income),
     main = "Monthly net income",
     xlab = "Euro")


# Which kind of inferences can we make with regards to the population,
# based on the sample data?
mean(df$income) # Sample mean is estimate for population mean
sd(df$income)/sqrt(length(df$income)) 
# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

# (b.) Confidence intervals --------
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?
mean(df$income) # Point estimate
sd(df$income)/sqrt(length(df$income)) # Standard error

# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 

# The **approximate** solution 
# Lower bound, 95 confidence level
lower_95 = mean(df$income) - (1.96*sd(df$income)/sqrt(length(df$income)))


# Upper bound, 95 confidence level
upper_95 = mean(df$income) + (1.96*sd(df$income)/sqrt(length(df$income)))


# Print
lower_95
mean(df$income)
upper_95


# The **precise** solution, using normal distribution
# Lower bound, 95 confidence level
lower_95_n <- qnorm(0.025, 
                    mean = mean(df$incom), 
                    sd = (sd(df$income)/sqrt(length(df$income))))

# Upper bound, 95 confidence level
upper_95_n <- qnorm(0.975,
                    mean = mean(df$income),
                    sd = (sd(df$incom)/sqrt(length(df$income))))


# Step by step
?qnorm
qnorm(0.025) # value for first 2.5%
qnorm(0.975) # value last 2.5%
qnorm(0.025, mean = 2, sd = 0.4) # Change mean and standard error


# Print
lower_95_n
mean(df$income)
upper_95_n


# How to calculate 99% confidence intervals?
# When to use normal distribution and when to use t distribution?

# The **precise** solution, using t distribution
t_score <- qt(0.995, df=length(df$income)-1)
lower_99_t <- mean(df$income)-(t_score)*(sd(df$income)/sqrt(length(df$income)))
upper_99_t <- mean(df$income)+(t_score)*(sd(df$income)/sqrt(length(df$income)))

# Step by step 
?qt
# This gives the critical value for the t-statistics of the 0.5th quantile in the 
## lower tail of the t-distribution with length(df$income)-1) degree of freedom. 
qt(0.005, df = length(df$income)-1) # critical value for first 0.5%
qt(0.995, df = length(df$income)-1) # last 0.5%
qt(0.005, df = length(df$income)-1, lower.tail = FALSE) # last 0.5%
qt(0.005, df = length(df$income)-1, lower.tail = TRUE) # first 0.5%



# Print
lower_99_t
mean(df$income)
upper_99_t


# Update Histogram 
hist(df$income)
abline(v = mean(df$income), col = "black")
abline(v = lower_95, col="black", lty = "dashed")
abline(v = upper_95, col="black", lty = "dashed")

abline(v = lower_95, col="blue", lty = "dashed")
abline(v = upper_95, col="red", lty = "dashed")

# Is there a relationship between education and income?


# Scatter plot 
plot(df$income, df$edu)
plot(df$income, df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

plot(df$income, df$edu,
     col=1) 

plot(df$income, df$edu,
     col=2) 

plot(df$income, df$edu,
     col=3) 


# Improve visualization and save
png(file="scatter_plot.png")
plot(df$edu,
     df$income,
     col=df$cap+1,
     ylab="Monthly net income (in Euro)",
     xlab="University level education (in years)",
     main="The Relationship between Education and Income")


# Add legend
legend(0,3000, # x and y position of legend
       legend=c("Capital", "Non capital"),
       col=c("red", "black"),
       pch=1,        # Marker type (1 is default)
       cex = 0.5) 
dev.off()



# Boxplot
boxplot(df$income ~ df$cap, 
        main="Boxplot of Income by place of residence",
        ylab="Euro",
        xlab="Place of residence",
        names=c("Non capital", "Capital"))


# (c.) Significance test for a mean ------

# What is the average monthly income in Ireland
# According to a quick Google search, it is 3034
# How does our sample compare to the population, 
# being the working population in Ireland?

t.test(df$income, mu = 3034)
?t.test
t.test(df$income, mu = 3034, alternative = "less")

# We also found a much easier way to calculate the confidence intervals (!)
t.test(df$income, conf.level = 0.99, alternative = "two.sided")

# Let's double check
lower_99_t
mean(df$income)
upper_99_t


# (d.) Significance test for a difference in means ------

# On average, do people earn differently in the capital
# compared to people who do not reside in the capital?

# Calculate means for subgroups
mean(df[df$cap==0, ]$income)
mean(df[df$cap==1, ]$income)

# Step by step
df$cap 
df$cap==0 # Only consider cases with cap==0 
df[df$cap==0, ] # Subsetting rows accordingly
df[df$cap==0, ]$income # Access variable
df$income[df$cap==0]
mean(df[df$cap==0, ]$income) # Calculate mean
mean(df[df$cap==1, ]$income) 


# t-test
t.test(df$income ~ df$cap, alternative = "two.sided")
?t.test


# On average, do people earn more in the capital
# compared to people who do not reside in the capital?

# t-test
t.test(df$income ~ df$cap, alternative = "less")


t.test(df$income ~ df$cap, alternative = "greater")





