# Applied Statistical Analysis I      
# Tutorial 3: Contingency tables, correlation & bivariate regression                      


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


# Agenda
# (a.) Contingency tables
# (b.) Chi-square test
# (c.) Correlation
# (d.) Bivariate regression 

# Load data 
dm_not_tidy <- read.csv("movies.csv")

# First step, look at data
View(dm_not_tidy)
str(dm_not_tidy)
head(dm_not_tidy)
summary(dm_not_tidy)

# Research questions: 
# Do different genres receive varying critical appreciation?

# (a.) Contingency tables -------

# Load tidy version of data
# The data is prepared using the data_wraning.R script.
dm <- readRDS("movies_updated.rds")
str(dm)

# First step, look at data
View(dm)
class(dm$genre)
levels(dm$genre)

# Contingency table 
table(dm$genre, # Genre
      dm$critics_rating) # Rating

# Subset data, only consider Comedy, Documentary, Drama

# Option 1: 
# Dataframe subsetting: dm[rows, columns]
dm_s <- dm[dm$genre=="Comedy" |
           dm$genre=="Drama" |
           dm$genre=="Documentary", ]
View(dm_s)

# Step by step
dm$genre # Select column
# For each row, value=="Comedy" or "Drama" or "Documentary"?
dm$genre=="Comedy" | dm$genre=="Drama" | dm$genre=="Documentary" 
# Select rows with value=="Comedy" or "Drama" or "Documentary"
dm[dm$genre=="Comedy" |
   dm$genre=="Drama" |
   dm$genre=="Documentary", ] 


# Option 2: Tidyverse subset

# Install and load tidyverse

dm_s <- subset(dm, dm$genre %in% c("Comedy","Documentary","Drama"))
View(dm_s)
?subset

# Step by step
dm$genre # Select column
"Horror" %in% c("Comedy","Documentary","Drama") # Horror is in vector?
"Comedy" %in% c("Comedy", "Documentary","Drama") # Comedy is in vector?
dm$genre  %in% c("Comedy","Documentary","Drama") # For each row, value==Comedy or Documentary or Drama?


# Contingency table 
table(dm_s$genre, # Genre
      dm_s$critics_rating) # Rating

# Problem: Although we filtered our data 
# the underlying levels still exist. Getting rid of
# these, we use the droplevels-function.
class(dm_s$genre)
levels(dm_s$genre)
dm_s$genre <- droplevels(dm_s$genre)


# Contingency table 
table(dm_s$genre, # Genre
      dm_s$critics_rating) # Rating


# Add marginal distributions
addmargins(table(dm_s$genre, # Genre
                 dm_s$critics_rating)) # Rating


# Joint probability 
prop.table(table(dm_s$genre, 
                 dm_s$critics_rating))

# Interpret as estimated probabilities of two specific
# values of each of the variables co-occurring together

# What is the probability of a Comedy and "Rotten"?
63/444


# (A) Conditional probability 
# What is the probability of "Rotten", 
# conditional on Comedy?
?prop.table()

# Over rows --> Rating conditional on genre
prop.table(table(dm_s$genre, # rows
                 dm_s$critics_rating), # columns
           margin = 1) # over rows

63/87

# Add marginal distributions
# Over rows --> Rating conditional on genre
addmargins(prop.table(table(dm_s$genre, 
                            dm_s$critics_rating), 
                      margin = 1)) # over rows

# Round
round(addmargins(prop.table(table(dm_s$genre, 
                       dm_s$critics_rating), 
                 margin = 1)), 2)

# Step by step 
round(0.72413793, 2) # Round to two decimals

# (B) Conditional probability 
# What is the probability of Comedy, 
# conditional on "Rotten?

# Over columns --> Genre conditional on rating
addmargins(prop.table(table(dm_s$genre, # rows
                            dm_s$critics_rating), # columns
                      margin = 2)) # over columns
63/190 

# Bar plot
barplot(prop.table(table(dm_s$genre, 
                         dm_s$critics_rating), margin=1),
        xlab = "Ranking",
        ylab = "Proportions",
        main = "Critics Rating by Genre",
        beside = TRUE,
        legend.text = TRUE,
        args.legend = list(x = 12, 
                           y = 0.7, 
                           cex = 0.8, 
                           box.col = "white"))

png(filename = "barplot.png",
    width = 600,
    height = 350)
barplot(prop.table(table(dm_s$genre, 
                         dm_s$critics_rating), margin=1),
        xlab="Ranking",
        ylab="Proportions",
        main="Critics Rating by Genre",
        beside=TRUE,
        legend.text = TRUE,
        args.legend = list(x=12, 
                           y=0.7, 
                           cex = 0.8, 
                           box.col = "white"))
dev.off()


# (b) Chi (kai) square test ------------

# Run Chi square test
chisq.test(dm_s$genre, 
           dm_s$critics_rating)


# Check p-value
sprintf("%.20f",1.097e-12)

# Step 2: Hypotheses
# Step 3: Test statistic
# Step 4: P-value
# Step 5: Conclusion

# A little side note, look at residuals
chi <- chisq.test(dm_s$genre, 
                  dm_s$critics_rating)
?chisq.test
str(chi)
# Returns the Pearson residuals, (observed - expected) / sqrt(expected)
chi$residuals


# (c.) Correlation -----

# Is there an association between education and income?

# Load data 
df <- read.csv("fictional_data.csv")

# Scatter plot 
plot(df$edu, df$income)
plot(df$edu, df$income,
     col=df$cap+1) # Color over third variable (+1, because first color in R is white)

# Improve visualization and save
png(file="scatter_plot.png")
plot(df$edu,
     df$income,
     col = df$cap+1,
     ylab = "Monthly net income (in Euro)",
     xlab = "University level education (in years)",
     main = "The Relationship between Education and Income")

# Add legend
legend(0, 3000, # x and y position of legend
       legend = c("Capital", "Non capital"),
       col = c("red", "black"),
       pch = 1,        # Marker type (1 is default)
       cex = 0.5) 
dev.off()


# Calculate correlation
cor(df$edu, df$income)

# Add to scatter plot
plot(df$edu, df$income)
text(6.5, 1000, sprintf("Correlation=%s", round(cor(df$income, df$edu), 4)))


# (d.) Bivariate regression  -----

# Is there a relationship between education and income?
regression1 <- lm(df$income ~ df$edu)
summary(regression1)

# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("regression_output1.tex", regression1)






