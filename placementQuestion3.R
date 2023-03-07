# Clean environment to ensure data are updated
rm(list = ls())
# Clean plot graphs
if(!is.null(dev.list())) dev.off()



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2", "stringr"))
# library(crayon)
library(dplyr)
library(ggplot2)
# library(reshape2)
library(stringr)



# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



# Question 3 - What will affect students to have paid classes?
educationLevel <- c(
  "No education",
  "Primary education",
  "Secondary education",
  "Degree Level",
  "Post Graduate"
)



# - Mother education (F.Polygon)
# - Father education (F.Polygon)
# - Mother current job (F.Polygon)
# - Father current job (F.Polygon)
# - Family support (Histogram)
# - Internet access (Histogram)
# - Address (Histogram) 
# - Curricular activities (Histogram)