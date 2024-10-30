# Applied Statistical Analysis I      
# Tutorial 8: Multiple regression                   

# Remove objects
rm(list=ls())

# Detach all libraries
detachAllPackages <- function() {
    basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", 
                        "package:datasets", "package:methods", "package:base")
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
lapply(c("stargazer", "vioplot", "arm"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Research questions: 
# What is the relationship between education and Euroscepticism?

# Subsetting data -----------

# Make sure to download ESS data first and 
# add to the datasets folder in your repository. 
# Download the ESS10 - integrated file, edition 3.2 here:
# https://ess-search.nsd.no/en/study/172ac431-2a06-41df-9dab-c1fd8f3877e7

# Look at the Codebook: 
# **Y** (euftf), European unification go further or gone too far
# 0: Unification already gone too far, 10: Unification go further
# **X1** (edlvdie), Highest level of education, Ireland
# **X2** (eduyrs), Years of full-time education completed

# Z1 (hinctnta), Household's total net income, all sources
# Unit is deciles (ranging between 1 and 10th deciles)
# Z2 (trstplt), Trust in politicians
# 0: No trust at all, 10: Complete trust
# Z3 (imwbcnt), Immigrants make country worse or better place to live
# 0: Worse place to live, 10: Better place to live

# Some general socio-demographic controls
# Gender (gndr), 1: Male, 2: Female
# Age (agea), Age of respondent, calculated
# Born in country (brncntr), 1: Yes, 2: No

# Only include Ireland and relevant variables. 
df <- read.csv("ESS10_Edi3.2.csv")
head(df)
c("euftf","edlvdie","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr") %in% 
  names(df)
c("euftf","edlvdie","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr") %in% 
  names(df[df$cnty == "IE", ])
sort(names(df))
sort(unique(df$edlvdis))

df_s <- df[df$cntry=="IE", 
           c("euftf","edlvdie","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
View(df_s)
head(df_s)

table(df$eduyrs)
# 0    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20   21 
# 99   18   40  169  742  328  561  386 2041 1790 1478 3488 7492 3494 2321 2518 2978 2249 1955  900  872  294 
# 22   23   24   25   26   27   28   29   30   31   32   35   36   37   40   42   45   48   50   51   54   55 
# 236  152  114   95   38   15   14    5   20    2    5    1    2    1   19    3    3    2    2    1    1    1 
# 65   77   88   99 
# 1  263  335   67 


# Reverse euftf, to measure euroscepticism more intuitively
df_s["euftf_re"] <- 10 - df_s[ ,c("euftf")]


# Categorize education levels
df_s["edu_cat"] <- NA
df_s[(df_s$edlvdie==1) | (df_s$edlvdie==2) | (df_s$edlvdie==3) | (df_s$edlvdie==4), c("edu_cat")] <- 1 # Junior Cycle
df_s[(df_s$edlvdie==5) | (df_s$edlvdie==6) | (df_s$edlvdie==7) | (df_s$edlvdie==8) | (df_s$edlvdie==9), c("edu_cat")] <- 2 # Leaving Certificate 
df_s[(df_s$edlvdie==10) | (df_s$edlvdie==11) | (df_s$edlvdie==12), c("edu_cat")] <- 3 # Advanced Certificate
df_s[(df_s$edlvdie==13) | (df_s$edlvdie==14) | (df_s$edlvdie==15), c("edu_cat")] <- 4 # Bachelor Degree
df_s[(df_s$edlvdie==16) | (df_s$edlvdie==17) | (df_s$edlvdie==18), c("edu_cat")] <- 5 # Postgraduate Degree


# Convert into factor variable
df_s$edu_cat <- factor(df_s$edu_cat, 
                       levels = c(1,2,3,4,5),
                       labels = c("Junior Cycle",
                                  "Leaving Certificate",
                                  "Advanced Certificate",
                                  "Bachelor Degree",
                                  "Postgraduate Degree"))
levels(df_s$edu_cat)
typeof(df_s$edu_cat)
is.factor(df_s$edu_cat)
table(df_s$edu_cat)
summary(df_s$edu_cat)


# Record missing values
df_s[(df_s == -67) | (df_s == -78) | (df_s == -89) | (df_s == 77) | (df_s == 88) | (df_s == 99) 
     | (df_s == 5555) | (df_s == 7777) | (df_s == 8888) | (df_s == 9999)] <- NA

summary(df_s)

# Descriptive plots
vioplot(df_s$euftf_re ~ df_s$edu_cat)
plot(df_s$edlvdie,df_s$euftf_re)
plot(jitter(df_s$edlvdie,2), jitter(df_s$euftf_re,2))

# Simple model only considering socio-demographic variables
model_base <- lm(euftf_re~gndr + agea + brncntr, data=df_s)
summary(model_base)

# (1) Hypothesis 1 --------------

# The higher the years of education, 
# the lower the level of Euroscepticism

# Continuous explanatory variable
model1 <- lm(euftf_re~edlvdie,data=df_s)
summary(model1)


# (2) Hypothesis 2 --------------

# The higher the income, 
# the lower the level of Euroscepticism.

model2 <- lm(euftf_re~hinctnta, data=df_s)
summary(model2)

# (3) Hypothesis 3 --------------

# The higher the trust in politics, 
# the lower the level of Euroscepticism.

model3 <- lm(euftf_re~trstplt, data=df_s)
summary(model3)

# (4) Hypothesis 4 --------------

# The more positive attitudes towards immigration, 
# the lower the level of Euroscepticism.

model4 <- lm(euftf_re~imwbcnt, data=df_s)
summary(model4)

# (5) Putting it all together ------------

# Continuous explanatory variable
model1 <- lm(euftf_re~edlvdie, data=df_s)
summary(model1)

# Add economic dimension
model_eco <- lm(euftf_re~edlvdie + hinctnta, data=df_s)
summary(model_eco)

# Add political dimension
model_pol <- lm(euftf_re~edlvdie + hinctnta + trstplt, data=df_s)
summary(model_pol)

# Add cultural dimension
model_cul <- lm(euftf_re~edlvdie+ hinctnta + trstplt + imwbcnt, data=df_s)
summary(model_cul)

# Add socio-economic variables 
model_final <- lm(euftf_re~edlvdie+ hinctnta + trstplt + imwbcnt + gndr + agea + brncntr, data=df_s)
summary(model_final)

# Get Latex table
stargazer(model1, model_eco,model_pol, model_cul, model_final)

# text table
stargazer(model1, model_eco,model_pol, model_cul, model_final, type = "text")


# Export Latex table 
# now save that output to a file that you can read in later to your answers
# make it easier for when we need to do this again, let's create a function
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file = outputFile, append = TRUE)
}
# execute function and check ls() to make sure it worked
output_stargazer("model.tex", model1, model_eco,model_pol, model_cul, model_final)

# How to visualize results?
coefplot(model_final)
coefplot(model1, add=TRUE, col.pts="gray")








