
# Applied Statistical Analysis I      
# Tutorial 1: Introduction & stats review                        

# Get working directory
getwd()

# Set working directory 
setwd("D:/TCD_courses/2024_2025Autumn/Applied StatsQuant Methods I_Fall 2024/tutorials/01")
getwd()

### Data collection ----------

# Load data 

df <- read.csv("fictional_data.csv")

# First step, look at data
View(df)
str(df) # Present structure of R object
?str

head(df)

tail(df, 2)

### Descriptive analysis ----------

### Income ###
hist(df$income) # Distribution
mean(df$income) # Central tendency, mean
var(df$income) # Variability, variance
sd(df$income) # Variability, standard deviation
sd(df$income)/sqrt(length(df$income)) # Variability, standard **error**

# Step by step
df$income
length(df$income)
sqrt(length(df$income))
sqrt(19)
sd(df$income)/sqrt(length(df$income))

### Education ###
hist(df$edu) # Distribution
mean(df$edu) # Central tendency, mean
var(df$edu) # Variability, variance
sd(df$edu) # Variability, standard deviation
sd(df$edu)/sqrt(length(df$edu)) # Variability, standard **error**

# Get summary statistics for entire dataset
summary(df)


###Sampling###
s <- sample(df$income, size = 2000, replace = TRUE, prob = df$edu)

hist(s, breaks = 100, xlab = "Income", ylab = "Frequency", main = "Income in one sample (N=2000)")

# Mean of the population
print(weighted.mean(df$income, w = df$edu), digits = 6)
## 2244.33

# Mean of the one sample
print(mean(s), digits = 6)
## 2242.42

# Which kind of inferences can we make with regards to the population,
# based on the sample data?
mean(df$income) # Sample mean is estimate for population mean
sd(df$income)/sqrt(length((df$income))) 
# Standard **error** (Sample standard deviation adjusted by sample size)
# is estimate for standard deviation of the sampling distribution

# Why do we need standard error again? --> to calculate measures of
# uncertainty for our point estimate (e.g., confidence intervals, and p-values)

### 95% Confidence level ###
# Definition: Point estimate +/- Margin of error, 
# where margin of error is a multiple of the standard error

# What do we need?
mean(df$income) # Point estimate
sd(df$income)/sqrt(length((df$income))) # Standard error

# How to find the multiple?
# Looking at the normal distribution, we see that 
# 95% of observations lie within +/-1.96 (approximately 2)
# standard errors of point estimate 


# Lower bound
lower_95 = (mean(df$income))-(1.96)*(sd(df$income)/sqrt(length(df$incomed)))


# Upper bound
upper_95 = (mean(df$income))+(1.96)*(sd(df$income)/sqrt(length(df$income)))


# Print
lower_95
mean(df$income)
upper_95


### Histogram ###
hist(df$income)
abline(v=mean(df$income),col="black")
abline(v=lower_95,col="black",lty="dashed")
abline(v=upper_95,col="black",lty="dashed")


# How to calculate 99% confidence intervals?


x <- rnorm(200, mean = 0, sd = 1) 
mean(x)
## -0.07337356

sd(x)
## 0.9675932

x <- rnorm(1000000, mean = 0, sd = 1) 
mean(x)
## -0.000101218

sd(x)
## 1.000413



#For the standard normal distribution, find the area to the left of x = 1.96
pnorm(q = 1.96)
## 0.9750021

pnorm(q = -1.96)
## 0.0249979

qnorm(0.025) ## -1.959964
qnorm(0.975) ## 1.959964


qnorm(0.005) ## -2.575829
qnorm(0.995) ## 2.575829


# Lower bound
lower_99 = (mean(df$income))-(2.58)*(sd(df$income)/sqrt(length((df$income))))


# Upper bound
upper_99 = (mean(df$income))+(2.58)*(sd(df$income)/sqrt(length((df$income))))


# Print
lower_99
mean(df$income)
upper_99



n <- length(df$income)

qt(0.025, n-1, lower.tail = T)
## -2.100922


# Is there a relationship between education and income?

### Scatter plot ###
plot(df$income, df$edu)
plot(df$income,df$edu,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Improve visualization and save
png(file="output/scatter_plot.png")
plot(df$edu,
     df$income,
     col=df$cap+1,
     ylab="Monthly net income (in Euro)",
     xlab="University level education (in years)",
     main="The Relationship between Education and Income")

# Add legend
legend(0,3000, # x and y position of legend
       legend=c("Capital", "Non capital"),
       col=c("black","red"),
       pch=1,        # Marker type (1 is default)
       cex = 0.5) 
dev.off()

















