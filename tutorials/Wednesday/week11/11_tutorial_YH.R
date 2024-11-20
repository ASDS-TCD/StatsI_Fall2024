# Applied Statistical Analysis/Quantitative Methods I     
# Tutorial 11: Transforming Variables, Collinearity, and Diagnostics

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
lapply(c("car"),  pkgTest)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Agenda
# (1) Influential cases/outliers
# (2) OLS assumptions
#     - Normality
#     - Constant variance
#     - Linearity
#     - Multicollinearity 


# Research question: 
# What is the relationship between education and Euroscepticism?

# Subsetting data -----------

# Make sure to download ESS data first and 
# add to the datasets folder in your repository. 
# Download the ESS11 - integrated file, edition 1.0 here:
# https://ess.sikt.no/en/datafile/242aaa39-3bbb-40f5-98bf-bfb1ce53d8ef/110?tab=0

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
df <- read.csv("ESS11.csv")
df_s <- df[df$cntry == "IE", c("euftf", "edlvdie", "eduyrs", "hinctnta",
                               "trstplt", "imwbcnt", "gndr", "agea", "brncntr")]
head(df_s)

# Reverse euftf, to measure euroscepticism more intuitively
df_s["euftf_re"] = 10 - df_s[ ,c("euftf")]


# Categorize education levels
df_s["edu_cat"] <- NA
df_s[(df_s$edlvdie==1) | (df_s$edlvdie==2) | (df_s$edlvdie==3) | (df_s$edlvdie==4), c("edu_cat")] <- 1 # Junior Cycle
df_s[(df_s$edlvdie==5) | (df_s$edlvdie==6) | (df_s$edlvdie==7) | (df_s$edlvdie==8) | (df_s$edlvdie==9), c("edu_cat")] <- 2 # Leaving Certificate 
df_s[(df_s$edlvdie==10) | (df_s$edlvdie==11) | (df_s$edlvdie==12), c("edu_cat")] <- 3 # Advanced Certificate
df_s[(df_s$edlvdie==13) | (df_s$edlvdie==14) | (df_s$edlvdie==15), c("edu_cat")] <- 4 # Bachelor Degree
df_s[(df_s$edlvdie==16) | (df_s$edlvdie==17) | (df_s$edlvdie==18), c("edu_cat")] <- 5 # Postgraduate Degree

summary(df_s$edu_cat)


# Convert into factor variable
df_s$edu_cat <- factor(df_s$edu_cat, 
                       levels = c(1,2,3,4,5),
                       labels = c("Junior Cycle",
                                  "Leaving Certificate",
                                  "Advanced Certificate",
                                  "Bachelor Degree",
                                  "Postgraduate Degree"))
levels(df_s$edu_cat)
#[1] "Junior Cycle"         "Leaving Certificate"  "Advanced Certificate" "Bachelor Degree"      "Postgraduate Degree" 

typeof(df_s$edu_cat)
is.factor(df_s$edu_cat)

summary(df_s$edu_cat)

summary(df_s)

# Recode missing values
df_s[(df_s == -67) | (df_s == -78) | (df_s == -89) | (df_s == 77) | (df_s == 88) | (df_s == 99) |
       (df_s == 999) | (df_s == 5555) | (df_s == 7777) | (df_s == 8888) | (df_s == 9999)] <- NA

# Save dataset
write.csv(df_s, "ess_euroscepticism.csv")


# Load data
df <- read.csv("ess_euroscepticism.csv", row.names="X")
head(df)

# Convert categorical variables into factor 
df$edu_cat <- factor(df$edu_cat)
df$gndr <- ifelse(df$gndr == 2, 1, 0)
df$gndr <- factor(df$gndr, labels = c("Male", "Female"))
df$brncntr <- ifelse(df$brncntr == 2, 1, 0)
df$brncntr <- factor(df$brncntr, labels = c("Born in country", "Not born in country"))

# Complete case analysis
df_na <- df[complete.cases(df), ] 

# Reset index
rownames(df_na) <- 1:nrow(df_na) 

# Final model
model_edu <- lm(euftf_re ~ eduyrs, data = df_na)

summary(model_edu)

model_final <- lm(euftf_re ~ eduyrs + 
                             hinctnta + 
                             trstplt + 
                             imwbcnt + 
                             gndr + 
                             agea + 
                             brncntr, data = df_na)

summary(model_final)

# (1) Influential cases/outliers ---------------

### Cook's Distance ###
# Difference in predicted values when observation
# i is included and not included
# Threshold > 4/(n-k-1)

# Get Cook's Distance for all observations
cooks_d <- cooks.distance(model_final)



# Plot 
par(mar = c(5,4,3,3)) # Reset figure margins

plot(model_final)

plot(model_final, which = 4)

# Get top 10 highest Cook's Distance values
head(sort(cooks_d, decreasing = TRUE), 10)
  
# Calculate threshold
thres <- 4/(nobs(model_final) - (length(coef(model_final)) - 1) - 1)

# Get observations above threshold
plot(model_final, which = 4, cook.levels = thres)
abline(h = thres, lty = 2, col = "red")

which(sort(cooks_d, decreasing = TRUE) > thres)


# What to do now?
# Investigate case by case. Coding error? Omitted variables?

# Subsetting data frames, df[row,column]
df_na[124, c("euftf_re", "eduyrs", "hinctnta", "trstplt", "imwbcnt", "gndr", "agea", "brncntr")]
             
            
model_final$fitted.values[124] # Predicted outcome


df_na[422, c("euftf_re", "eduyrs", "hinctnta", "trstplt", "imwbcnt", "gndr", "agea", "brncntr")]

model_final$fitted.values[422] # Predicted outcome


df_na[509, c("euftf_re", "eduyrs", "hinctnta", "trstplt", "imwbcnt", "gndr", "agea", "brncntr")]

model_final$fitted.values[509] # Predicted outcome

### Difference in betas ####
# Difference in coefficients when observation i is included and not included

# We repeat the same process

# Get DFBeta for all observations
dfbeta <- dfbeta(model_final)
head(dfbeta)

# Print results for some observations
dfbeta[1, c("eduyrs")]
dfbeta[2, c("eduyrs")]

sprintf("%.10f", dfbeta[1, c("eduyrs")])


# Find maximum absolute values for each coefficient 
dfbeta[, c("eduyrs")][which.max(abs(dfbeta[, c("eduyrs")]))]

dfbeta[, c("hinctnta")][which.max(abs(dfbeta[, c("hinctnta")]))]

dfbeta[, c("trstplt")][which.max(abs(dfbeta[, c("trstplt")]))]

dfbeta[, c("imwbcnt")][which.max(abs(dfbeta[, c("imwbcnt")]))]

# What to do now?
# Investigate case by case. Coding error? Omitted variables?

# Subsetting data frames, df[row,column]
df_na[124, c("euftf_re", "eduyrs", "hinctnta", "trstplt", "imwbcnt", 
             "gndr", "agea", "brncntr")]

model_final$fitted.values[124] # Predicted outcome

df_na[644,c("euftf_re", "eduyrs", "hinctnta", "trstplt", "imwbcnt", 
            "gndr", "agea", "brncntr")]

model_final$fitted.values[644] # Predicted outcome

### Residuals vs Leverage plot ###
# Leverage: Unusual value on X
# Discrepancy: Unusual value on Y, given value on X
# Influence = Leverage x Discrepancy
# --> unusual value on X and Y 


# Plot: Residuals vs Leverage 
plot(model_final, which = 5)


# Look at case with very high leverage out of curiosity
# but has low discrepancy, so it is not an influential case



# Plot: Studentized Residuals vs Hat Values
plot(hatvalues(model_final), rstudent(model_final), type = 'n')
cook_m_final <- sqrt(cooks.distance(model_final))
points(hatvalues(model_final), rstudent(model_final), 
       cex = (10 * cook_m_final)/max(cook_m_final))
abline(h = c(-2, 0, 2), lty = 2)
abline(v = c(2, 3) * 3/dim(df_na)[1], lty = 2)


# Studentized Residuals

head(sort(rstudent(model_final)))

tail(sort(rstudent(model_final)))


## Bonferroni correction is multiplying the p-values by the number of residuals

outlierTest(model_final, row.names(df_na))



# Plot the hat values against the indices 1 to dim(df_na)[1], and include
## thresholds for 2(k + 1)/n and 3(k + 1)/n

dim(df_na)[1]

(2 * (7 + 1))/dim(df_na)[1]

(3 * (7 + 1))/dim(df_na)[1]

which(hatvalues(model_final) > (2 * (7 + 1))/dim(df_na)[1])

which(hatvalues(model_final) > (3 * (7 + 1))/dim(df_na)[1])


plot(1:dim(df_na)[1], hatvalues(model_final), pch = 16, cex = 0.6)
abline(h = (2 * (7 + 1))/dim(df_na)[1], lty = 2, col = "blue")
abline(h = (3 * (7 + 1))/dim(df_na)[1], lty = 2, col = "red")
text(1:dim(df_na)[1], hatvalues(model_final), labels = row.names(df_na), pos=1, cex = 0.6)


plot(1:dim(df_na)[1], hatvalues(model_final), pch = 16, cex = 0.6)
abline(h = (2 * (7 + 1))/dim(df_na)[1], lty = 2, col = "blue")
abline(h = (3 * (7 + 1))/dim(df_na)[1], lty = 2, col = "red")
identify(1:dim(df_na)[1], hatvalues(model_final), labels = row.names(df_na), pos=1, cex = 0.6)


plot(1:dim(df_na)[1], hatvalues(model_final), pch = 16, cex = 0.6)
abline(h = (2 * (7 + 1))/dim(df_na)[1], lty = 2, col = "blue")
abline(h = (3 * (7 + 1))/dim(df_na)[1], lty = 2, col = "red")
text(which(hatvalues(model_final) > (3 * (7 + 1))/dim(df_na)[1]), 
     hatvalues(model_final)[hatvalues(model_final) > (3 * (7 + 1))/dim(df_na)[1]], 
     labels = which(hatvalues(model_final) > (3 * (7 + 1))/dim(df_na)[1]), 
     pos=1, cex = 0.6)


df_na[927, c("euftf_re", "eduyrs", "hinctnta", "trstplt",
             "imwbcnt", "gndr", "agea", "brncntr")]

model_final$fitted.values[927] # Predicted outcome

# What to do now?
# Investigate case by case. Coding error? Omitted variables?


# (2) OLS assumptions ---------------

### Normality ###
# The error is normally distributed 

# Histogram of error
hist(model_final$residuals)

boxplot(model_final$residuals)

# QQ plot: (Quantile-quantile) plot
plot(model_final)

plot(model_final, which = 2)

### Constant variance ###
# The error has a constant variance (homoscedasticity)

# Residuals vs Fitted plot
plot(model_final)

plot(model_final, which = 1)

plot(model_final$fitted.values, model_final$residuals)
text(model_final$fitted.values[abs(model_final$residuals) > 
                                 quantile(model_final$residuals, 0.975)], 
     model_final$residuals[abs(model_final$residuals) > 
                             quantile(model_final$residuals, 0.975)], 
     labels = row.names(df_na)[abs(model_final$residuals) > 
                                 quantile(model_final$residuals, 0.975)])


# What to do if labels of observations are overlapping?

which(abs(model_final$residuals) > quantile(model_final$residuals, 0.995) 
      & abs(model_final$fitted.values) > quantile(model_final$fitted.values, 0.995))
#124

which(abs(model_final$residuals) > quantile(model_final$residuals, 0.975) 
      & abs(model_final$fitted.values) > quantile(model_final$fitted.values, 0.975))


model_final$residuals["992"] #7.121697
model_final$fitted.values["992"] #2.878303 

model_final$residuals["424"] #-0.9207229 
model_final$fitted.values["424"] #6.920723  

model_final$residuals["124"] #-7.613692
model_final$fitted.values["124"] #7.613692 


plot(model_final$fitted.values, model_final$residuals)
identify(model_final$fitted.values, model_final$residuals, 
         labels = row.names(df_na),
         pos=1, cex = 0.6)


### Linearity ###
# The effect between X and Y is linear

# Scatter plots 


plot(df_na$eduyrs, jitter(df_na$euftf_re, 2))

plot(df_na$hinctnta, jitter(df_na$euftf_re, 2))

plot(df_na$trstplt, jitter(df_na$euftf_re, 2))

plot(df_na$imwbcnt, jitter(df_na$euftf_re, 2))

plot(df_na$agea, jitter(df_na$euftf_re, 2))

# Residual plot
residualPlots(model_final)

# Add a quadratic term for trust in politics
df_na$trstplt_trstplt <- df_na$trstplt^2

# Fit model
model_quad <- lm(euftf_re ~ eduyrs + 
                            hinctnta + 
                            trstplt + 
                            trstplt_trstplt +
                            imwbcnt +         
                            gndr + 
                            agea + 
                            brncntr, data=df_na)

summary(model_quad)

# Compare residual plot for quadratic model
residualPlots(model_quad)

boxplot(model_quad$residuals)

boxplot(model_final$residuals, model_quad$residuals)


# We might also want to log-transform education years. 
# This variable is right/positively skewed. 
hist(df_na$eduyrs) 

# Log-transform education years
# +1 because log(0) = -Inf
hist(log(df_na$eduyrs + 1)) 

min(df_na$eduyrs)

log(0)

# Fit model
model_log <- lm(euftf_re ~ log(eduyrs + 1) + 
                           hinctnta + 
                           trstplt + 
                           imwbcnt +         
                           gndr + 
                           agea + 
                           brncntr, data=df_na)

summary(model_log)

# Compare residual plot for log model
residualPlots(model_log)

boxplot(model_edu$residuals, model_final$residuals, model_quad$residuals, model_log$residuals)


# But be careful, if we transform X we need to 
# adjust interpretation. There is a trade-off between
# fit and interpretability. 

### Multicollinearity ###
# Independent variables are strongly correlated

# Correlation matrix
cor(df_na[, c("eduyrs", "hinctnta", "trstplt", "imwbcnt", "agea")])

# Variance Inflation Factor
vif(model_final)

# Create a variable with high correlation
cor(df_na$trstplt, df_na$imwbcnt)

df_na$trust_att <- df_na$trstplt + df_na$imwbcnt

cor(df_na$trust_att,df_na$trstplt)

cor(df_na$trust_att,df_na$imwbcnt)

# Refit model with highly correlated variables
model_collin <- lm(euftf_re ~ eduyrs + 
                              hinctnta + 
                              trstplt + 
                              imwbcnt +
                              trust_att, data = df_na)

summary(model_collin)

boxplot(df_na$eduyrs, log(df_na$eduyrs + 1), df_na$edu_cat)


boxplot(model_edu$residuals, model_final$residuals, model_quad$residuals, 
        model_log$residuals, model_collin$residuals)



