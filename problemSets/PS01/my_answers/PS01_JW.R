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


setwd("C:/Users/julia/OneDrive/Desktop/R Data")
getwd()

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

mean_y <- mean(y)

se_y <- sd(y) / sqrt(length(y))

critical_value <- 1.645

upper_90 <- (mean(y)) + (critical_value) * (sd(y) / sqrt(length(y)))

lower_90 <- (mean(y)) - (critical_value) * (sd(y) / sqrt(length(y)))

lower_90
mean_y
upper_90

t_test_result <- t.test(y, mu = 100, alternative = "greater", conf.level = 0.95)

t_test_result


#####################
# Problem 2
#####################

#task 2 exercise 1

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

pkgTest <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE)) stop("Package not found")
  }
}

required_packages <- c("ggplot2", "corrplot", "GGally")

lapply(required_packages, pkgTest)

library(GGally)
ggpairs(expenditure, columns = c("Y", "X1", "X2", "X3"))

library(corrplot)
cor_matrix <- cor(expenditure[, c("Y", "X1", "X2", "X3")], use = "complete.obs")

print(cor_matrix)
pdf()
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
dev.off()

#task 2 exercise 2
library(ggplot2)

expenditure$Region <- factor(expenditure$Region, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Northeast", "North Central", "South", "West"))

ggplot(expenditure, aes(x = Region, y = Y)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Per Capita Expenditure on Housing Assistance by Region",
       x = "Region",
       y = "Per Capita Expenditure (Y)") +
  theme_minimal()



#problem 2 exercise 3 a

library(ggplot2)

ggplot(expenditure, aes(x = X1, y = Y)) +
  geom_point(color = "blue", size = 3) +  # Scatterplot with blue points
  labs(title = "Relationship Between Per Capita Expenditure (Y) and Income (X1)",
       x = "Per Capita Personal Income (X1)",
       y = "Per Capita Expenditure on Housing Assistance (Y)") +
  theme_minimal()



#problem 2 exercise 3 b
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

library(ggplot2)

expenditure$Region <- factor(expenditure$Region, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Northeast", "North Central", "South", "West"))

ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +
  geom_point(size = 3) +  
  labs(title = "Relationship Between Per Capita Expenditure (Y), Income (X1) by Region",
       x = "Per Capita Personal Income (X1)",
       y = "Per Capita Expenditure on Housing Assistance (Y)") +
  theme_minimal() +
  theme(legend.title = element_text(face = "bold"),
        legend.position = "right")