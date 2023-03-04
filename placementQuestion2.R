# Clean environment to ensure data are updated
rm(list = ls())



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2"))
# library(crayon)
# library(dplyr)
library(ggplot2)
# library(reshape2)

# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



# Question 2 - What will affect students to get grades higher than 70 marks in secondary school and higher secondary school?
# - Mother education

# - Father education

# - Mother current job

# - Father current job

# - Family Support

# - Paid class

# - Curricular activities

# - Internet Access

# - Secondary eduboard

# - Higher secondary eduboard
