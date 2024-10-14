# Applied Statistical Analysis I/Quantitative Methods I         
# Tutorial 5: Bivariate regression                   


# remove objects
rm(list = ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list) > 0)  for (package in package.list) detach(package,  character.only = TRUE)
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

lapply(c("wbstats", "tidyverse", "ggplot2", "stargazer", "reshape2"),  pkgTest) #dplyr

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

getwd()

# Agenda 
# (a.) Gathering data
# (b.) Data wrangling
# (c.) Descriptive analysis
# (d.) Regression analysis

# Research questions: 
# Is there a relationship between income and child mortality?

## "wbstats", "tidyverse", "ggplot2", "stargazer"

# (a.) Gathering data ----------

# What is an API?
# https://medium.com/geekculture/a-beginners-guide-to-apis-9aa7b1b2e172

# Load data from World Bank API 
# require("wbstats")
wb <- wb_data(country = c("AF","BRA","ITA","NGA","SWE","UGA"), 
              indicator = c("NY.GDP.PCAP.CD", # GDP per capita (current US$)
                     "SP.POP.TOTL", # Population, total 
                     "SE.SEC.ENRR", #  School enrollment, secondary (% gross) 
                     "SH.DYN.MORT"), # Mortality rate, under-5 (per 1,000 live births) 
              start_date = 2000, end_date = 2023)

dim(wb) #144 8
head(wb)
tail(wb)
names(wb)

write.csv(wb, "wb.csv")


# Data formats--Wide and long
# https://www.statology.org/long-vs-wide-data/


# Reshape data from wide to long (put colums in rows) #melt needs pkg reshape2
wb_re <- melt(select(wb, iso2c, iso3c, country, date,   
                     NY.GDP.PCAP.CD, SE.SEC.ENRR, SH.DYN.MORT, SP.POP.TOTL),
                 id.vars = c("iso2c", "iso3c", "country", "date"),
                 variable.name = "indicatorID", #"NY.GDP.PCAP.CD", "SE.SEC.ENRR", "SH.DYN.MORT", "SP.POP.TOTL"
                 value.name = "value") 
                

dim(wb_re) #576 6
tail(wb_re)
#       iso2c iso3c country date indicatorID value
# 571    UG   UGA  Uganda 2018 SP.POP.TOTL 41515395
# 572    UG   UGA  Uganda 2019 SP.POP.TOTL 42949080


colnames(wb)[5:8] <- c("value.GDPPC", "value.SEC", 
                       "value.MORT", "value.POP")
names(wb)


wb_long <- reshape(wb, direction = "long",
                varying = 5:8,
                v.names = "value",
                timevar = "indicatorID",
                idvar = c("iso2c", "iso3c", "country", "date"),
                sep = ".")

head(wb_long)
tail(wb_long)



# Reshape data from long to wide (put rows in columns) 

wb_wide <- reshape(wb_re[, c("country","iso3c","date","indicatorID","value")], # df
                 timevar = "indicatorID", # New columns
                 idvar = c("country","date","iso3c"), # Identifiers for rows
                 direction = "wide")
head(wb_wide)


# Load Quality of Government data
qog <- read.csv("https://www.qogdata.pol.gu.se/data/qog_bas_ts_jan23.csv")
write.csv(qog, "qog.csv")
qog <- read.csv("qog.csv")

names(qog)
head(qog[, c("year", "ccodealp", "bmr_dem")])
summary(qog$bmr_dem)

# How can we combine data from different sources?
# https://guides.nyu.edu/quant/merge

head(wb_re$date)
summary(wb_re$date)
summary(qog$year)

# Merge 
df <- merge(wb, # Left df
            qog[, c("ccodealp","year","bmr_dem")], # Right df
            by.x=c("date","iso3c"), # Merge variables in left
            by.y=c("year","ccodealp"), # Merge variables in right
            all.x=TRUE, # Merge operation, only keep left
            sort=FALSE) # Do not sort observations

# Rename columns
names(df)
names(df)[5] <- "gdp_per_cap"
names(df)[6] <- "pop_size"
names(df)[7] <- "sec_enrol"
names(df)[8] <- "mort"
names(df)[9] <- "democracy"
View(df)

str(df) #144 9

# Save df
write.csv(df, "df_income_mortality.csv")

# (b.) Data wrangling -------

# Load df
df <- read.csv("df_income_mortality.csv")
View(df)

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

head(df_s)

# Get the mean income and max child mortality for each year
df_grouped <- group_by(df, date) # Group by year
df_mean_inc <- summarize(df_grouped, 
                         n=n(), # Counts
                         mean_inc=mean(gdp_per_cap), # Mean
                         max_mort=max(mort)) # Max
df_mean_inc

# Check if df has missing values
sum(is.na(df$gdp_per_cap))
sum(is.na(df$mort))
df$gdp_per_cap

# Replace missing values 

# Option I: Replace missing values with zero, but be careful!
df_na <- df %>% replace(is.na(.), 0)
df_na$gdp_per_cap

# Option II: Replace missing values with mean
df_na <- df # Copy
df_na$gdp_per_cap
?replace_na
df_na$gdp_per_cap <- replace_na(data=df_na$gdp_per_cap, 
                                replace=mean(df_na$gdp_per_cap, # Value to replace NA with
                                             na.rm = TRUE))
# Step by step: 
mean(df_na$gdp_per_cap, na.rm = TRUE)

df_na$gdp_per_cap

# Option III: Replace missing values with group mean
df_na <- group_by(df_na, country) # Group
df_na <- mutate(df_na, # Replace with mean if value is missing
                sec_enrol = ifelse(is.na(sec_enrol), 
                                   mean(sec_enrol, na.rm = TRUE), 
                                   sec_enrol))
head(df_na)
tail(df_na)

# Step by step:
# ifelse(test, yes, no) 
# --> if is.na is True, replace with mean,
# if is.na is False, replace with value

df_na$sec_enrol = ifelse(is.na(df_na$sec_enrol), 
                   mean(df_na$sec_enrol, na.rm = TRUE), 
                   df$sec_enrol)

# Re-coding variables, in Base R
# Create categorical income variable
dim(df_na) #144 11
df_na$income_group <- rep(0, 144) # Create a new variable with 144 0s
summary(df_na$gdp_per_cap) # Check quantile
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 142.9   763.0  3099.4 15718.6 32143.0 61417.7 
quantile(df_na$gdp_per_cap)
# 0%        25%        50%        75%       100% 
# 142.9034   763.0543  3099.4425 32142.9991 61417.6809 
df_na$income_group[df_na$gdp_per_cap > 763.0] <- 1 # Place step by step
df_na$income_group[df_na$gdp_per_cap > 3099.4] <- 2
df_na$income_group[df_na$gdp_per_cap > 32142.9991] <- 3


# Convert into factor
typeof(df_na$income_group)

df_na$income_group <- factor(df_na$income_group, 
                           labels = c("low","medium_low","medium_high","high"))

typeof(df_na$income_group)

class(df_na$income_group)

is.factor(df_na$income_group)

table(df_na$income_group)

summary(df_na$income_group)

# Re-coding variables, in tidyverse
# Create categorical income variable
quantile(df_na$gdp_per_cap) # Check quantiles
df_na <- mutate(df_na, income_group2 = cut(gdp_per_cap, 
                                       breaks = quantile(df_na$gdp_per_cap),
                                       labels = c("low","medium_low","medium_high","high")))

typeof(df_na$income_group2)

summary(df_na$income_group2)

df_na[is.na(df_na$income_group2) == TRUE, ]

df_na$income_group2[is.na(df_na$income_group2) == TRUE] <- "low"

summary(df_na$income_group2)

# Step by step: 
cut(df_na$gdp_per_cap, breaks = c(0, 600, 800, Inf)) # Define breaks
quantile(df_na$gdp_per_cap)
cut(df_na$gdp_per_cap, breaks = quantile(df_na$gdp_per_cap)) # Use quantiles as breaks
head(df_na$gdp_per_cap)
cut(df_na$gdp_per_cap, breaks = quantile(df_na$gdp_per_cap),
                      labels=c("low","medium_low","medium_high","high")) # Add labels
df_na$income_group2[df_na$gdp_per_cap == min(df_na$gdp_per_cap)] <- "low"

summary(df_na$income_group2)

# Drop missing values 
df <- df[complete.cases(df), ]


# (c.) Descriptive analysis -------

# Key components of a ggplot:
# 1. data 
# 2. aesthetic mappings --> aes()
# 3. layers, each layer has geometric object --> geoms

# For example: 
# ggplot(data = <data>, mapping = aes(<mappings>)) + --> data, with aesthetic mapping
#     <geom_function>() + --> geometric object

# Scatter plot
scatter <-
  ggplot(data = df_na, # --> data
         mapping = aes(x = gdp_per_cap, 
             y = mort)) +  # --> aesthetic mapping
  geom_point() # --> geometric object, scatter plot

# Print plot object
scatter

# Scatter plot, log-transform income
hist(df_na$gdp_per_cap)
hist(log(df_na$gdp_per_cap))

# Scatter plot
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort)) + 
  geom_point() 
scatter

# Scatter plot 
scatter <-
  ggplot(data = df_na, 
         mapping = aes(x = log(gdp_per_cap), # log-transform
                       y = mort,
                       size = sec_enrol)) + 
  geom_point() 
scatter

# Improve visualization and save
scatter <- ggplot(data = df_na, 
       mapping = aes(x = log(gdp_per_cap), # log-transform
                     y = mort,
                     size = sec_enrol)) + 
  geom_point()  +
  labs(x = "GDP per capita (log)", # Add labels
       y = "Child mortality",
       linewidth = "Population size") +
  theme_classic() + # Change theme
  theme(legend.box.background = element_rect(size = 0.1), # Change background
        legend.position.inside = c(0.85, 0.85)) # Change position of legend
ggsave(scatter, file = "scatter.png")
scatter

# (d.) Regression analysis -----

# Fit model
model <- lm(mort ~ gdp_per_cap, data=df_na)
summary(model)

# Re-scale income
df_na$gdp_per_cap_1000 <- df_na$gdp_per_cap/1000
model <- lm(mort ~ gdp_per_cap_1000, data=df_na)
summary(model)

stargazer(model)

# Export Latex table 
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("model.tex", model)


# How to include a categorical independent variable?

# Fit model
model <- lm(mort ~ democracy, data = df_na)
summary(model)





