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
# library(stringr)



# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



placementData[is.na(placementData)] <- 0



# Question 4 - What will affect students' salary?
# - Secondary education board

# - Secondary grade

# - Higher secondary education board

# - Higher secondary grade

# - Higher secondary specialization

# - Degree grade

# - Degree specialization

# - Master grade

# - Work experiences

# - Employment test

# - Employment specialization
