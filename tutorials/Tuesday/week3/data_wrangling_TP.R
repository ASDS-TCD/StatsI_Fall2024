################################
# Movies data wrangling script #
################################

# Install and load packages
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

lapply(c("lubridate", "tidyverse"),  pkgTest)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# check working directory
getwd()

# Import movies.csv
df <- read.csv("movies.csv") 

### Content 
# (a.) Convert character variables into factor variables
# (b.) Convert yes/no into logical values
# (c.) Collapse date information into one variable

# (a.) Convert character variables into factor variables ----

# Check which variables in df are character
char_vecs <- sapply(df, is.character) 

# Get values for each character variable
unique <- Map(length, lapply(df[,char_vecs], unique)) 

# Subset character variables with >2 and <=12 unique values
factors <- names(df[,char_vecs][,unique <= 11 & unique > 2]) 

# Convert these into factor variable
df[,factors] <- lapply(df[,factors], as.factor) 

# Re-level factors, aka change order of levels
# For mpaa_rating
levels(df$mpaa_rating)
df$mpaa_rating <- factor(df$mpaa_rating, 
                         levels = c("G", "PG", "PG-13", # new level order
                                    "R", "NC-17", "Unrated"))
levels(df$mpaa_rating)

# And for critics_rating
levels(df$critics_rating)
df$critics_rating <- factor(df$critics_rating, 
                            levels = c("Rotten", "Fresh", # new level order
                                       "Certified Fresh"))
levels(df$critics_rating)

# Convert audience_rating into factor (not included in subset above)
df$audience_rating <- factor(df$audience_rating, 
                             levels = c("Spilled", "Upright"))

# (b.) Convert yes/no into logical values ------

# Check which variables in df are logical
logical <- names(df[,char_vecs][,unique == 2]) 

# Always check the outcome of your coding.
# audience_rating is in fact not a logical variable

# Drop one column (audience_rating) which is not logical
logical <- logical[-1] 

# Recode "yes"/"no" to TRUE/FALSE
df[,logical] <- ifelse(df[,logical] == "no", FALSE, TRUE) 

# Convert into logical variables 
df[,logical] <- lapply(df[,logical], as.logical) 

# (c.) Collapse date information into one variable -----

# Create date variable 
df$thtr_rel <- make_date(df$thtr_rel_year,
                         df$thtr_rel_month,
                         df$thtr_rel_day)
?make_date 

# Create date variable 
df$dvd_rel <- make_date(df$dvd_rel_year,
                        df$dvd_rel_month,
                        df$dvd_rel_day)

# Remove original variables
df <- df[, !names(df)  %in% c("thtr_rel_year",
                              "thtr_rel_month",
                              "thtr_rel_day",
                              "dvd_rel_year",
                              "dvd_rel_month",
                              "dvd_rel_day")]

# Save tidy version of df
saveRDS(df, "movies.rds")

