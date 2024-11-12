# Applied Statistical Analysis I      
# Tutorial 11: Multiple regression, Interactions                  

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
lapply(c("stargazer","vioplot","broom","fastDummies","arm","emmeans","ggplot2"),  pkgTest)

# Set working directory for current folder
setwd("C:/Users/tpa064/Downloads")
getwd()

# Agenda
# (1) Regression analysis
# (2) F test for some coefficient
# (3) Interactions
# (4) Quadratic effects

# Research questions: 
# What is the relationship between education and Euroscepticism?

# Load data
df <- read.csv("C:/Users/tpa064/Downloads/ESS10.csv")
View(df)

df_s <- df[df$cntry=="IE", c("euftf","edlvdie","eduyrs","hinctnta","trstplt","imwbcnt","gndr","agea","brncntr")]
View(df_s)

# Reverse euftf, to measure euroscepticism more intuitively
df_s["euftf_re"] = 10 - df_s[ ,c("euftf")]

# Categorize education levels
df_s["edu_cat"] <- NA
df_s[(df_s$edlvdie==1) | (df_s$edlvdie==2) | (df_s$edlvdie==3) | (df_s$edlvdie==4), c("edu_cat")] <- 1 # Junior Cycle
df_s[(df_s$edlvdie==5) | (df_s$edlvdie==6) | (df_s$edlvdie==7) | (df_s$edlvdie==8) | (df_s$edlvdie==9), c("edu_cat")] <- 2 # Leaving Certificate 
df_s[(df_s$edlvdie==10) | (df_s$edlvdie==11) | (df_s$edlvdie==12), c("edu_cat")] <- 3 # Advanced Certificate
df_s[(df_s$edlvdie==13) | (df_s$edlvdie==14) | (df_s$edlvdie==15), c("edu_cat")] <- 4 # Bachelor Degree
df_s[(df_s$edlvdie==16) | (df_s$edlvdie==17) | (df_s$edlvdie==18), c("edu_cat")] <- 5 # Postgraduate Degree

# Convert into factor variable
df_s$edu_cat <- factor(df_s$edu_cat)
is.factor(df_s$edu_cat)

# (1) Regression analysis ----------

# Complete case analysis
df_na <- df_s[complete.cases(df_s), ] 

# Categorical independent variable (manually)

# Create dummy variables 
df_na$JuniorCycle <- ifelse(df_na$edu_cat == "Junior Cycle", 1, 0)
df_na$LeavingCertificate <- ifelse(df_na$edu_cat == "Leaving Certificate", 1, 0)
df_na$AdvancedCertificate <- ifelse(df_na$edu_cat == "Advanced Certificate", 1, 0)
df_na$Bachelor <- ifelse(df_na$edu_cat == "Bachelor Degree", 1, 0)
df_na$Postgraduate <- ifelse(df_na$edu_cat == "Postgraduate Degree", 1, 0)

df_na <- dummy_cols(df_na, select_columns = "edu_cat")

View(df_na)

# Fit model (ref=JuniorCycle)
model <- lm(euftf_re~edu_cat_1+edu_cat_2+edu_cat_3+edu_cat_4,data=df_na)
summary(model)

# Change reference category to leaving certificate 
model <- lm(euftf_re~JuniorCycle+AdvancedCertificate+Bachelor+Postgraduate,data=df_na)
summary(model)

# Education--Categorical independent variable
is.factor(df_na$edu_cat)
model1 <- lm(euftf_re~edu_cat,data=df_na)
summary(model1)

# Change reference category to leaving certificate 
levels(df_na$edu_cat) # First level, "Advanced Certificate"
df_na$edu_cat <- relevel(df_na$edu_cat, ref = 4)
levels(df_na$edu_cat) # First level, "Leaving Certificate"

# Refit model
model1 <- lm(euftf_re~edu_cat,data=df_na)
summary(model1)

# Education--Continuous independent variable
model1 <- lm(euftf_re~eduyrs,data=df_na)
summary(model1)

# Add economic dimension
model_eco <- lm(euftf_re~eduyrs + hinctnta,data=df_na)
summary(model_eco)

# Add political dimension
model_pol <- lm(euftf_re~eduyrs + hinctnta + trstplt, data=df_na)
summary(model_pol)

# Add cultural dimension
model_cul <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt, data=df_na)
summary(model_cul)

# Add socio-economic variables 
model_final <- lm(euftf_re~eduyrs + hinctnta + trstplt + imwbcnt + gndr + agea + brncntr, data=df_na)
summary(model_final)

# Get Latex table
stargazer(model1,model_eco,model_pol,model_cul,model_final)

# How to visualize results?
coefplot(model_final) # 95% Confidence intervals (and 50% CIs)
coefplot(model1, add=TRUE, col.pts="gray")

# F test for some coefficients -------

# Does adding economic dimension improve fit?
anova(model1, model_eco, test='F')
summary(model_eco)

# In which cases is partial F test not equal to t-test?

# Does adding political dimension improve fit?
anova(model1, model_pol, test='F')
summary(model_pol)

# What about political dimension alone?
model3 <- lm(euftf_re~eduyrs+trstplt,data=df_na) 
anova(model1, model3, test='F')
summary(model3)

# Does adding the education dummy set improve fit?
model5 <- lm(euftf_re~hinctnta + trstplt + imwbcnt, data=df_na)
model6 <- lm(euftf_re~hinctnta + trstplt + imwbcnt + edu_cat, data=df_na)
anova(model5, model6, test='F')
summary(model6)

# In conclusion: When to use partial F test?

# (3) Interactions ------------

# So far, visualizing regression models with categorical independent variables 

# Fit model
model1 <- lm(euftf_re~edu_cat+imwbcnt,data=df_na)

# Plot
plot(df_na$imwbcnt, df_na$euftf_re)
abline(model1$coefficients[1], model1$coefficients[6],col="black") # edu_cat=0, "Leaving Certificate"
abline(model1$coefficients[1]+model1$coefficients[2], model1$coefficients[6],col="blue") # edu_cat=1, "Advanced Certificate"
abline(model1$coefficients[1]+model1$coefficients[3], model1$coefficients[6],col="green") # edu_cat=2, "Bachelor"
abline(model1$coefficients[1]+model1$coefficients[4], model1$coefficients[6],col="gray") # edu_cat=3, "Junior Cycle"
abline(model1$coefficients[1]+model1$coefficients[5], model1$coefficients[6],col="orange") # edu_cat=4, "Postgraduate"
legend(7, 10, # Legend
       legend=c("Leaving Certificate","Advanced Certificate","Bachelor","Junior Cycle","Postgraduate"),
       col=c("black","blue","green","gray","orange"),
       pch=1) 

# In ggplot
ggplot(df_na, aes(imwbcnt, euftf_re, group = edu_cat)) +
  geom_point(aes(colour = edu_cat)) + # Add points
  geom_line(data = df_na, aes(y = model1$fitted.values, colour = edu_cat)) # Add regression lines

# A. Add interaction term between, attitudes towards immigration and gender
# (categorical x continuous)

# Recode dummy variables as 0,1; needed for interactions
df_na$gndr <- ifelse(df_na$gndr == 2, 1, 0)

# And convert into factor
df_na$gndr <- factor(df_na$gndr, labels = c("Male", "Female"))

# Fit model
model_int <- lm(euftf_re~eduyrs + 
                  imwbcnt + 
                  gndr + 
                  imwbcnt*gndr, data=df_na)
summary(model_int)

# What is the prediction equation?
# How to interpret the intercept?
# How to interpret the coefficient for education?
# How to interpret the coefficient for attitudes towards immigration?
# How to interpret the coefficient for gender?
# How to interpret the interaction term?

# Get slopes for levels of categorical variables
summary(model_int)$coefficients[3]
summary(model_int)$coefficients[3]+summary(model_int)$coefficients[5]

# Or use emmeans package
emtrends(model_int, ~ gndr, var="imwbcnt")

# Visualize estimated marginal means (EMMs)
emmip(model_int, 
      gndr ~ imwbcnt,
      at=list(imwbcnt=seq(0,10,by=1), gndr=c("Male","Female"), eduyrs=mean(df_na$eduyrs)),
      plotit = TRUE, 
      CIs = TRUE)

# What does this function do?
emmip(model_int, 
      gndr ~ imwbcnt,
      at=list(imwbcnt=seq(0,10,by=1), gndr=c("Male","Female"), eduyrs=mean(df_na$eduyrs)),
      plotit = FALSE, 
      CIs = TRUE)

# We can also use predict
predict(model_int, 
        newdata=data.frame(imwbcnt=0,gndr="Male",eduyrs=mean(df_na$eduyrs)))

# Prediction equation from model with interaction term
# 7.15015+0.01875*Education-0.47287*Attitudes-1.10277*Gender+0.16751*Attitudes*Gender

# Calculate the marginal mean, for men, with very negative attitudes
# towards immigration, and mean education level.

# What is the marginal effect of changing gender from male to female, in this scenario? 

# B. Add interaction term between, education and whether the person was born in country
# (categorical x categorical)

# Recode dummy variables as 0,1; needed for interactions
df_na$brncntr <- ifelse(df_na$brncntr == 2, 1, 0)

# And convert into factor
df_na$brncntr <- factor(df_na$brncntr, labels = c("Born in country", "Not born in country"))

# Fit model
model_int2 <- lm(euftf_re~edu_cat + 
                   imwbcnt + 
                   brncntr +
                   edu_cat*brncntr, data=df_na)
summary(model_int2)


# Partial F test for interactions
# Does the interaction effect improve fit?
model_no_int <- lm(euftf_re~edu_cat + imwbcnt + brncntr, data=df_na)
anova(model_no_int, model_int2)

# (4) Quadratic effects -----------

# A. Add a quadratic education term
df_na$eduyrs_eduyrs <- df_na$eduyrs^2

# Is the effect of education quadratic, rather than linear?
model_quad <- lm(euftf_re~eduyrs + 
                   eduyrs_eduyrs + 
                   hinctnta + 
                   trstplt + 
                   imwbcnt, data=df_na)
summary(model_quad)

# Visualize quadratic effect

# Make predictions; How to specify new data?
sort(unique(df_na$eduyrs))
sort(unique(df_na$eduyrs_eduyrs))
mean(df_na$hinctnta)

# Define new data, for which to make predictions
new_data = data.frame(eduyrs=sort(unique(df_na$eduyrs)), # Education
                      eduyrs_eduyrs=sort(unique(df_na$eduyrs_eduyrs)), # Quadratic education term
                      hinctnta=mean(df_na$hinctnta), # Income
                      trstplt=mean(df_na$trstplt), # Political trust
                      imwbcnt=mean(df_na$imwbcnt)) # Attitudes towards immigration
new_data

# Make predictions for new data
preds <- predict(model_quad, newdata=new_data)
preds

# Scatter plot
par(mar = c(5, 5, 2, 2)) # Change margins in plot manually
plot(jitter(df_na$eduyrs,2),jitter(df_na$euftf,2))
lines(sort(unique(df_na$eduyrs)),preds) # Add predicted outcomes

# B. Add a quadratic income term
df_na$hinctnta_hinctnta <- df_na$hinctnta^2

# Is the effect of income quadratic, rather than linear?
model_quad2 <- lm(euftf_re~eduyrs + 
                    hinctnta + 
                    hinctnta_hinctnta, 
                  data=df_na)
summary(model_quad2)

# Visualize quadratic effect

# Define new data, for which to make predictions
new_data = data.frame(hinctnta=sort(unique(df_na$hinctnta)), # Income
                      hinctnta_hinctnta=sort(unique(df_na$hinctnta_hinctnta)), # Quadratic income term
                      eduyrs=mean(df_na$eduyrs)) # Education
new_data

# Make predictions for new data
preds <- predict(model_quad2, newdata=new_data)
preds

# Scatter plot
par(mar = c(5, 5, 2, 2)) # Change margins in plot manually
plot(jitter(df_na$hinctnta,2), jitter(df_na$euftf,2))
lines(sort(unique(df_na$hinctnta)), preds) # Add predicted outcomes


