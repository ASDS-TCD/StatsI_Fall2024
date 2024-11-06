# Applied Statistical Analysis I/Quantitative Methods I     
# Tutorial 9: Multivariable Regression 2                   


# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# Load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load any necessary packages
lapply(c("wbstats", "tidyverse", "dplyr", "ggplot2", "stargazer", "readr", "reshape2"),  pkgTest)


# Set working directory as path where file is located
getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Agenda 
# (a.) Gathering data
# (b.) Introduction to tidyverse
# (c.) Descriptive analysis
# (d.) Regression analysis

# Research questions: 
# Is there a relationship between income and child mortality?
# Install and load packages
# Adopted from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them


# (a.) Data wrangling ----------

# What is an API?
# https://medium.com/geekculture/a-beginners-guide-to-apis-9aa7b1b2e172

# Load data from World Bank API
wb <- wb_data(country = c("AF","BRA","ITA","NGA","SWE","UGA"), 
              indicator = c("NY.GDP.PCAP.CD", # GDP per capita (current US$)
                            "SP.POP.TOTL", # Population, total 
                            "SE.SEC.ENRR", #  School enrollment, secondary (% gross) 
                            "SH.DYN.MORT"), # Mortality rate, under-5 (per 1,000 live births) 
              start_date = 2000, end_date = 2023)

head(wb)


# Load Quality of Government data
qog <- read_csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")
write.csv(qog, "qog.csv")
qog <- read_csv("qog.csv")
# How can we combine data from different sources?
# https://guides.nyu.edu/quant/merge

# Merge 
df <- merge(wb, # Left df
            qog[, c("ccodealp","year","bmr_dem")], # Right df
            by.x=c("date","iso3c"), # merge variables in left
            by.y=c("year","ccodealp"), # merge variables in right
            all.x=TRUE, # merge operation, only keep left
            sort=FALSE) # Do not sort observations

# Rename columns
names(df)
names(df)[5] <- "gdp_per_cap"
names(df)[6] <- "pop_size"
names(df)[7] <- "sec_enrol"
names(df)[8] <- "mort"
names(df)[9] <- "democracy"
View(df)
head(df)

# Save df
write.csv(df, "df_income_mortality.csv")

# Exercise A -----------------
# Add another variable either from the World Bank data or 
# the Quality of Government dataset. 
# Alternatively, add a variable from a new data source. 

# Load zipped data from url
temp <- tempfile(fileext = ".zip") # Initiate temporary file
# Download zip as temporary file
download.file("https://ucdp.uu.se/downloads/ged/ged231-csv.zip", temp, mode="wb")
ucdp <- read_csv(temp) # Read data
View(ucdp)
write.csv(ucdp, "ucdp_ged231.csv")

# Load some data that you find interesting. 
# What is the level of analysis? 
# Or rather, what is the 'unit' of each row?

# Data on armed conflicts: 
# UCDP Georeferenced Event Dataset (GED)
# https://ucdp.uu.se/downloads/index.html#ged_global
View(ucdp)


# Other common data sources:  
# Afrobarometer (survey), https://www.afrobarometer.org/
# Example, Sierra Leone Round 8 data (2021) 
# Codebook: https://www.afrobarometer.org/wp-content/uploads/2022/06/afrobarometer_codebook_srl_r8_en_2021-06-18.pdf

# Load sav data file 

# Install required package
if(!require(haven)){
  install.packages("haven")
  library(haven)}

# Load sav file
afro <- read_sav("https://www.afrobarometer.org/wp-content/uploads/2023/06/SRL_R9.data_.final_.wtd_release.14Feb23.sav")
write.csv(afro, "afro_202306_SRL.csv")
# Look at data
View(afro)
afro$Q1

# European Social Survey (ESS), https://www.europeansocialsurvey.org/
# European Commission, https://data.europa.eu/en
# American National Election Survey (ANES), https://electionstudies.org/
# Information on api: https://github.com/jamesmartherus/anesr
# Comparative Legislators Database, https://complegdatabase.com/
# Information api: https://cran.r-project.org/web/packages/legislatoR/vignettes/legislatoR.html
# Irish Polling Indicator, https://pollingindicator.com/#data
# General Social Survey Cumulative Data, Codebook: https://gss.norc.org/documents/codebook/gss_codebook.pdf
# Information on api: https://kjhealy.github.io/gssr/

# How can we merge data on different levels of analysis?

# What is the level of analysis?
View(ucdp)
View(df)

# How to aggregate data?
# Which variables do we need?
head(ucdp[, c("country", "year", "best")])

# Aggregate ucdp from event to the country-year level. 
ucdp_agg <- aggregate(ucdp$best, # Variable to aggregate
                      list(ucdp$country, ucdp$year), # Group variables
                      FUN=sum) # How to aggregate, sum
head(ucdp_agg)

# Merge 
df <- merge(df, # Left df
            ucdp_agg, # Right df
            by.x=c("date","country"), # merge variables in left
            by.y=c("Group.2","Group.1"), # merge variables in right
            all.x=TRUE, # merge operation, only keep left
            sort=FALSE) # Do not sort observations

# Rename column
names(df)
names(df)[10] <- "best"
View(df)

# Why do we see missing values?
# Missing values mean that country-year was not in UCDP data, 
# so there were no fatalities. 
# We can replace with 0s. 
summary(df$best)
df$best[is.na(df$best)] = 0

# Step by step
df$best # Select variable
is.na(df$best) # Check if value is na
df$best[is.na(df$best)] # Subset rows which are na
df$best[is.na(df$best)] = 0 # Replace with 0

# Save df
write.csv(df, "df_income_mortality_best.csv")

# (b.) Data wrangling -------

# Load df
df <- read_csv("df_income_mortality_best.csv")
View(df)
head(df)
table(df$country)

# Get unique countries in df
df_uni <- select(df, country) # Select variable
df_uni <- distinct(df_uni, country) # Get unique values
df_uni

# Get unique countries in df, using the pipe
df %>% 
  select(country) %>% 
  distinct(country) 

# Filter (subset is base R)
df_s <- filter(df, country %in% c("Afghanistan","Italy")) 
df_s

# Get the mean income and max child mortality for each year
df_grouped <- group_by(df, date) # Group by year
df_mean_inc <- summarize(df_grouped, 
                         n=n(), # Counts
                         mean_inc=mean(gdp_per_cap), # Mean
                         max_mort=max(mort)) # Max
df_mean_inc

# What about missing values?


# Check if df has missing values
sum(is.na(df$gdp_per_cap))
sum(is.na(df$mort))

# Replace missing values 

# Option I: Replace missing values with zero, but be careful!
df_na <- replace(df, is.na("gdp_per_cap"), 0) # one variable
df_na <- df %>% replace(is.na(.), 0) # all variables

# Option II: Replace missing values with mean
df_na <- df # Copy
?replace_na
df_na$gdp_per_cap <- replace_na(data=df_na$gdp_per_cap, 
                                replace=mean(df_na$gdp_per_cap, # Value to replace NA with
                                             na.rm = TRUE))
dim(df) #144 10
dim(df_na) #144 10

# Option III: Replace missing values with group mean
df_na <- group_by(df_na, country) # Group
df_na <- mutate(df_na, # Replace with mean if value is missing
                sec_enrol = ifelse(is.na(sec_enrol), 
                                   mean(sec_enrol, na.rm = TRUE), 
                                   sec_enrol))

# Step by step:
# ifelse(test, yes, no) 
# --> if is.na is True, replace with mean,
# if is.na is False, replace with value


# Re-coding variables, in Base R
# Create categorical income variable
df_na$income_group <- rep(0, dim(df_na)[1]) # Create empty variable
summary(df_na$gdp_per_cap) # Check quantile
summary(df_na$gdp_per_cap)[2] #763.0543 
summary(df_na$gdp_per_cap)[3] #3099.442 
summary(df_na$gdp_per_cap)[4]
summary(df_na$gdp_per_cap)[5]
df_na$income_group[df_na$gdp_per_cap>=summary(df_na$gdp_per_cap)[2]] <- 1 # Replace step by step
df_na$income_group[df_na$gdp_per_cap>=summary(df_na$gdp_per_cap)[3]] <- 2
df_na$income_group[df_na$gdp_per_cap>=summary(df_na$gdp_per_cap)[5]] <- 3

# Convert into factor
typeof(df_na$income_group)
df_na$income_group <- factor(df_na$income_group, 
                           labels = c("low","medium_low","medium_high","high"))
typeof(df_na$income_group)
is.factor(df_na$income_group)
levels(df_na$income_group)
unique(df_na$income_group)
table(df_na$income_group)
summary(df_na$income_group)

plot(df_na$income_group,df_na$gdp_per_cap)

# Re-coding variables, in tidyverse
# Create categorical income variable

df_na$income_group2 <- cut(df_na$gdp_per_cap, 
                                           breaks = 4, 
                                           labels = c("low","medium_low","medium_high","high"))

plot(df_na$income_group2,df_na$gdp_per_cap)


typeof(df_na$income_group2)
is.factor(df_na$income_group2)

summary(df_na$income_group2)

head(df_na[is.na(df_na$income_group2) == TRUE, ])

summary(df_na$income_group2)


# Step by step: 
summary(df_na$gdp_per_cap)
cut(df_na$gdp_per_cap, breaks=c(0,600,800,Inf)) # Define breaks
cut(df_na$gdp_per_cap, breaks= 4)
cut(df_na$gdp_per_cap, breaks= 4, 
                       labels=c("low","medium_low","medium_high","high")) # Add labels

# Drop missing values 
df <- df[complete.cases(df), ]

# Exercise B -----------------

# Get the mean income for democracies and autocracies. 

# Base R
mean(df_na[df_na$democracy==0,]$gdp_per_cap)

mean(df_na[df_na$democracy==1,]$gdp_per_cap)

# Step by step:
df_na[df_na$democracy==0,] # Subset rows

df_na[df_na$democracy==0,]$gdp_per_cap # Subset columns 

mean(df_na[df_na$democracy==0,]$gdp_per_cap) # Calculate mean

# Tidyverse:
df_grouped <- group_by(df_na, democracy) # Group by regime type

df_mean_inc <- summarize(df_grouped, 
                         mean_inc=mean(gdp_per_cap)) # Mean

df_mean_inc

# Get the mean income for Afghanistan. 

# Base R
mean(df_na[df_na$country=="Afghanistan",]$gdp_per_cap)

# Tidyverse: 
df_sub <- filter(df_na, country=="Afghanistan") 
df_sub <- summarise(df_sub, mean = mean(gdp_per_cap))
df_sub

# Create a variable, which measures the t-1 lag of GDP per capita.

# What is a lagged variable?
# "A lagged value of a variable is the value of the variable 
# from a previous time period. For instance, a lagged value 
# from one period previous to the current time is referenced
# as being from time t−1" (Kellstedt and Whitten 2018, p.257).

# Group data, lagged variable needs to be country specific
df_na_g <- arrange(df_na, country, date) # Important, sort rows beforehand!
df_na_g <- group_by(df_na_g, country) # Group by country
df_na <- mutate(df_na_g, 
                income_lag = lag(gdp_per_cap, n = 1)) # Calculate t-1 lag

# Use piping
df_na <- df_na %>%
  arrange(country, date)  %>%
  group_by(country) %>%
  mutate(income_lag = lag(gdp_per_cap, n = 1))

# Create a variable, which measure regime change. 

# Step 1: Create 'help' variable, lagged regime type
View(df_na) # Make sure data is sorted
df_na_g <- group_by(df_na, country) # Group data by country
head(df_na_g)
df_na <- mutate(df_na_g, 
                democracy_lag = lag(democracy, n = 1)) # Calculate t-1 lag

# Step 2: Recode
df_na$regime_change <- 0 # Create empty variable

# Replace with 1 if current regime type is the same as t-1 regime type
df_na$regime_change[df_na$democracy_lag!=df_na$democracy] <- 1 

# (c.) Descriptive analysis -------

# Key components of a ggplot:
# 1. data 
# 2. aesthetic mappings --> aes()
# 3. layers, each layer has geometric object --> geoms

# For example: 
# ggplot(data = <data>, mapping = aes(<mappings>)) + --> data, with aesthetic mapping
#     <geom_function>() + --> geometric object

# Scatter plot
scatter_gdppc_mort <-
  ggplot(data = df_na, # --> data
         mapping = aes(x = gdp_per_cap, 
                       y = mort)) +  # --> aesthetic mapping
  geom_point() # --> geometric object, scatter plot

# Print plot object
scatter_gdppc_mort

# Scatter plot, log-transform income
hist(df_na$gdp_per_cap)

hist(log(df_na$gdp_per_cap))

# Scatter plot
scatter_lngdppc_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort)) + 
  geom_point() 

scatter_lngdppc_mort

# Scatter plot 
scatter_lngdppc_mort_size <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort,
                       size = sec_enrol)) + 
  geom_point() 

scatter_lngdppc_mort_size

# Improve visualization and save
scatter_lngdppc_mort_size <- ggplot(data = df_na, 
                  mapping = aes(x = log(gdp_per_cap), # log-transform
                                y = mort,
                                size = sec_enrol)) + 
  geom_point()  +
  labs(x = "GDP per capita (log)", # Add labels
       y = "Child mortality",
       size = "Population size") +
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position = c(0.85, 0.85)) # Change position of legend

ggsave(scatter_lngdppc_mort_size, file = "scatter_lngdppc_mort_size.png")

scatter_lngdppc_mort_size

# Exercise C -----------------

# Add a regression line to your scatter plot. 

# Scatter plot
scatter_lngdppc_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap),
                       y = mort)) + 
  geom_point() +
  geom_smooth(method='lm',col="black") # Add regression line
scatter_lngdppc_mort


scatter_lngdppc_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = income_group2,
                       y = mort)) + 
  geom_point() +
  geom_smooth(method='lm',col="black") # Add regression line
scatter_lngdppc_mort


# Create a new "scatter plot", which differentiates between
# democracies and autocracies. Add the regression lines. 

# Scatter plot
scatter_dem_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = factor(democracy), # Over regime type
                       y = mort, 
                       col = factor(democracy))) + 
  geom_point() 
scatter_dem_mort

# Access regression coefficients
model <- lm(mort ~ democracy, data=df_na)
summary(model)
model$coefficients
model$coefficients[1]

# Add regression lines
scatter_dem_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = factor(democracy), # Over regime type
                       y = mort, 
                       col = factor(democracy))) + 
  geom_point() +
  geom_hline(yintercept=model$coefficients[1],col="red") + # Autocracies
  geom_hline(yintercept=model$coefficients[1]+model$coefficients[2],col="turquoise") # Democracies
scatter_dem_mort

# Equal to the mean differentiated by regime type
head(df_na)
mean(df_na[df_na$democracy==0,]$mort, na.rm = TRUE)
mean(df_na[df_na$democracy==1,]$mort, na.rm = TRUE)
model$coefficients[1]+model$coefficients[2]

# (d.) Regression analysis -----

df_na <- na.omit(df_na)

# Fit model
model <- lm(mort ~ gdp_per_cap, data=df_na)
summary(model)

# Re-scale income
df_na$gdp_per_cap_1000 <- df_na$gdp_per_cap/1000
model_mort_gdppc1000 <- lm(mort ~ gdp_per_cap_1000, data=df_na)
summary(model_mort_gdppc1000)

stargazer(model_mort_gdppc1000)

# Export Latex table 
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("mort_gdppc1000.tex", mort_gdppc1000)


# How to include a categorical independent variable?

# Fit model
model_mort_gdp1000_dem <- lm(mort ~ gdp_per_cap_1000 + democracy, data=df_na)
summary(model_mort_gdp1000_dem)

anova(model_mort_gdp1000_dem)


anova(model, model_mort_gdp1000_dem, test = "F")

model_mort_gdp1000_dem$coefficients

scatter_dem_mort <-
  ggplot(data = df_na, 
         mapping = aes(x = gdp_per_cap_1000, # Over regime type
                       y = mort, 
                       col = factor(democracy))) + 
  geom_point() +
  geom_smooth(method='lm',col="black") + 
  geom_hline(yintercept=model_mort_gdp1000_dem$coefficients[1],col="red") + # Autocracies
  geom_hline(yintercept=model_mort_gdp1000_dem$coefficients[1]+model_mort_gdp1000_dem$coefficients[3],col="turquoise") # Democracies
scatter_dem_mort



# Exercise D -----------------

# Estimate a regression model, which interests you. 
# What is you research question? How would you present
# your results?


# What is the relationship between income and armed conflict?




# How about the role of the regime type?











