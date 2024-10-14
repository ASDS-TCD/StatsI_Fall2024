################################
# Movies data wrangling script #
################################

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

lapply(c("tidyverse", "lubridate", "stargazer"),  pkgTest)

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()


# Import movies.csv
dm <- read.csv("movies.csv") 

View(dm)

str(dm)

### Content 
# (a.) Convert character variables into factor variables
# (b.) Convert yes/no into logical values
# (c.) Collapse date information into one variable

# (a.) Convert character variables into factor variables ----

# Check which variables in dm are character
char_vecs <- sapply(dm, is.character) 

char_vecs

# Get values for each character variable
unique <- Map(length, lapply(dm[,char_vecs], unique)) 

# Subset character variables with >2 and < 12 unique values
factors <- names(dm[,char_vecs][,unique < 12 & unique > 2]) 
factors <- names(dm[,char_vecs][,unique <= 11 & unique > 2]) 

# Convert these into factor variable
dm[,factors] <- lapply(dm[,factors], as.factor) 

head(dm[, factors])

str(dm[, factors])
dim(dm[, factors]) #651   4

# Re-level factors, aka change order of levels
# For mpaa_rating
levels(dm$mpaa_rating)

table(dm$mpaa_rating)
# G   NC-17      PG   PG-13       R  Unrated 
#19       2     118     133     329      50 

dm$mpaa_rating <- factor(dm$mpaa_rating, 
                          levels = c("Unrated", "G", "PG", "PG-13", # new level order
                                     "R", "NC-17"))

levels(dm$mpaa_rating)

table(dm$mpaa_rating)
# Unrated       G      PG   PG-13       R   NC-17 
# 50      19     118     133     329       2 

# And for critics_rating
levels(dm$critics_rating)
# "Certified Fresh" "Fresh"           "Rotten"   

dm$critics_rating_new <- factor(dm$critics_rating, 
                             levels = c("Rotten", "Fresh", # new level order
                                        "Certified Fresh"))

levels(dm$critics_rating_new)

str(dm)


# Convert audience_rating into factor (not included in subset above)
names(dm[,char_vecs][,unique == 2]) 

str(dm$audience_rating)

dm$audience_rating <- factor(dm$audience_rating, 
                              levels = c("Spilled", "Upright"))

# (b.) Convert yes/no into logical values ------

# Check which variables in dm are logical

logical <- names(dm[,char_vecs][,unique == 2]) 

# Always check the outcome of your coding.
# audience_rating is in fact not a logical variable

unique(dm$audience_rating)
unique(dm$best_pic_nom) 

# Drop one column (audience_rating) which is not logical
str(dm[, logical])
logical <- logical[-1] 

# Recode "yes"/"no" to TRUE/FALSE
dm[,logical] <- ifelse(dm[,logical] == "no", FALSE, TRUE) 

# Convert into logical variables 
dm[,logical] <- lapply(dm[,logical], as.logical) 

str(dm[, logical])
head(dm[, logical])
tail(dm[, logical])
unique(dm[, logical])


# (c.) Collapse date information into one variable -----

# Create date variable 
dm$thtr_rel_date <- make_date(dm$thtr_rel_year,
                         dm$thtr_rel_month,
                         dm$thtr_rel_day)
?make_date

# Create date variable 
dm$dvd_rel_date <- make_date(dm$dvd_rel_year,
                        dm$dvd_rel_month,
                        dm$dvd_rel_day)


# Remove original variables
dm <- dm[, !names(dm) %in% c("thtr_rel_year",
                             "thtr_rel_month",
                             "thtr_rel_day",
                             "dvd_rel_year",
                             "dvd_rel_month",
                             "dvd_rel_day")]

dm$dvd_rel_date

# Save tidy version of dm
saveRDS(dm, "movies_updated.rds")


View(dm)


