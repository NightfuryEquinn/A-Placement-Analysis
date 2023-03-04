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
educationLevel <- c(
  "No education",
  "Primary education",
  "Secondary education",
  "Degree Level",
  "Post Graduate"
)

# - Mother education
momEducation <- table(placementData$Medu)

momEduLevel <- as.vector(names(momEducation))
# Rename table entry with education level
names(momEduLevel) <- educationLevel
# Create empty vector
pass70ssc <- vector("numeric", length(educationLevel))
pass70hsc <- vector("numeric", length(educationLevel))

for(i in 1:length(educationLevel)) {
  count70ssc <- sum(placementData$Medu == i-1 & placementData$ssc_p >= 70)
  count70hsc <- sum(placementData$Medu == i-1 & placementData$hsc_p >= 70)
  
  pass70ssc[i] <- count70ssc
  pass70hsc[i] <- count70hsc
}

pass70ssc_data <- data.frame(
  Mother_Education <- educationLevel,
  Count <- pass70ssc
)

ggplot(pass70ssc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))

pass70hsc_data <- data.frame(
  Mother_Education <- educationLevel,
  Count <- pass70hsc
)

ggplot(pass70hsc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))

# - Father education

# - Mother current job

# - Father current job

# - Family Support

# - Paid class

# - Curricular activities

# - Internet Access

# - Secondary eduboard

# - Higher secondary eduboard
