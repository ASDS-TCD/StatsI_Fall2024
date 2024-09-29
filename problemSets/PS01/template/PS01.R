#####################
# load libraries
# set wd
# clear global .envir
#####################

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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t_test_result <- t.test(y, conf.level = 0.90)

ci_90 <- t_test_result$conf.int
cat("90%置信区间为：", ci_90, "\n")

t_test_hypothesis <- t.test(y, mu = 100, alternative = "greater")
p_value <- t_test_hypothesis$p.value
#####################
# Problem 2
#####################
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)
head(expenditure)
pairs(~ Y + X1 + X2 + X3, data = expenditure, main = "Scatterplot Matrix")
cor_matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")], use="complete.obs")
print(cor_matrix)
boxplot(Y ~ Region, data = expenditure, 
        xlab = "Region", ylab = "Per Capita Expenditure on Housing Assistance",
        main = "Per Capita Expenditure on Housing Assistance by Region")
region_means <- aggregate(Y ~ Region, data = expenditure, mean)
print(region_means)
plot(expenditure$X1, expenditure$Y, 
     xlab = "Per Capita Personal Income", 
     ylab = "Per Capita Expenditure on Housing Assistance", 
     main = "Relationship between Y and X1")
cols <- c("red", "blue", "green", "purple")
symbols <- c(16, 17, 18, 19)

plot(expenditure$X1, expenditure$Y, 
     xlab = "Per Capita Personal Income", 
     ylab = "Per Capita Expenditure on Housing Assistance", 
     main = "Relationship between Y and X1 by Region", 
     col = cols[expenditure$Region], 
     pch = symbols[expenditure$Region])
legend("topright", legend = c("Northeast", "North Central", "South", "West"), 
       col = cols, pch = symbols, title = "Region")

