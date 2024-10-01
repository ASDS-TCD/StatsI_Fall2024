#####################
# load libraries
# set wd
# clear global .envir
#####################

setwd("/Users/ellakaragulyan/Documents/StatsI_Fall2024/")
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

t.test(y, conf.level = 0.90, alternative = "two.sided")
t.test(y, mu = 100, alternative = "greater")
t.test(y, mu = 100)

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

attributes(expenditure$Y)$label <- "Per capita expenditure on shelters/housing assistance in state"
attributes(expenditure$X1)$label <- "Per capita personal income in state"
attributes(expenditure$X2)$label <- "Number of residents per 100,000 that are financially insecure in state"
attributes(expenditure$X3)$label <- "Number of people per thousand residing in urban areas in state"
expenditure$Region <- factor(expenditure$Region, 
                             levels = c(1, 2, 3, 4), 
                             labels = c("Northeast", "North Central", "Southeast", "West"))

str(expenditure)
summary_exp <- summary(expenditure)
summary_exp

# Plotting Y and X1
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_y_x1.png")
plot(expenditure$X1, expenditure$Y,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Per capita personal income",
     main="Relationship between expenditure on shelters/housing\nassistance and personal income in state")
dev.off()

# Plotting Y and X2
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_y_x2.png")
plot(expenditure$X2, expenditure$Y,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Number of financially insecure residents per 100,000",
     main="Relationship between per capita expenditure on shelters/housing\nassistance and number of financially insecure residents \nper 100,000 in state")
dev.off()

# Plotting Y and X3
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_y_x3.png")
plot(expenditure$X3, expenditure$Y,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Number of people per thousand residing in urban areas",
     main="Relationship between per capita expenditure on shelters/housing\nassistance and number of people per thousand residing \nin urban areas in state")
dev.off()

# Plotting X1 and X2
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_x1_x2.png")
plot(expenditure$X1, expenditure$X2,
     ylab="Per capita personal income",
     xlab="Number of financially insecure residents per 100,000",
     main="Relationship between per capita personal income and \nnumber of financially insecure residents per 100,000 in state")
dev.off()

# Plotting X1 and X3
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_x1_x3.png")
plot(expenditure$X1, expenditure$X3,
     ylab="Per capita personal income",
     xlab="Number of people per thousand residing in urban areas",
     main="Relationship between per capita personal income and \nnumber of people per thousand residing in urban areas in state")
dev.off()

# Plotting X2 and X3
png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_x2_x3.png")
plot(expenditure$X2, expenditure$X3,
     ylab="Number of financially insecure residents per 100,000",
     xlab="Number of people per thousand residing in urban areas",
     main="Relationship between number of financially insecure \nresidents per 100,000 and number of people \nper thousand residing in urban areas in state")
dev.off()

png(file="/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/boxplot_y_r.png")
boxplot(expenditure$Y ~ expenditure$Region, 
    main = "Per capita expenditure on shelters/housing assistance by region",
    ylab = "Per capita expenditure in state",
    xlab = "Region",
    names = c("Northeast", "North Central", "Southeast", "West")  
)
for (i in 1:length(means)) {
  text(i, means[i], labels = paste0("M = ", round(abs(means[i]), 2)), pos = 3, col="black", cex=0.8)
}
dev.off()

# Plotting Y and X1 by region
png(file = "/Users/ellakaragulyan/Documents/StatsI_Fall2024/problemSets/PS01_EK/scatterplot_y_x1_r.png")
plot(expenditure$X1, expenditure$Y, 
     col=c("aquamarine3","coral", "cornflowerblue", "darkolivegreen"),   # Use Region for color
      pch = 19,  # Solid circle for points
      ylab = "Per capita expenditure on shelters/housing assistance",
      xlab = "Per capita personal income",
      main = "Relationship between expenditure on shelters/housing\nassistance and personal income in state")
     legend("topright", legend = levels(as.factor(expenditure$Region)), 
            col=c("aquamarine3","coral", "cornflowerblue", "darkolivegreen"), 
          pch = 19, title = "Region")
dev.off()


#ADDING CORRELATION - OPTIONAL 
correlation <- cor(expenditure$X1, expenditure$Y, use = "complete.obs")
# Add the correlation as text to the scatterplot
text(x = max(expenditure$X1) * 0.9,  # Position text at 80% of the x-axis
     y = max(expenditure$Y) * 0.99,   # Position text at 90% of the y-axis
     labels = sprintf("Correlation = %.4f", correlation),
     col = "blue", cex = 1.2)  # Adjust text color and size (cex)

# Close the PNG device and save the file
dev.off()
