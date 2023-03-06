# Clean environment to ensure data are updated
rm(list = ls())



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



# Question 3 - What will affect students to have paid classes?
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
names(momEduLevel) <- educationLevel

momEducation_yes <- vector("numeric", length(educationLevel))
momEducation_no <- vector("numeric", length(educationLevel))

for(i in 1:length(educationLevel)) {
  count_momEducation_yes <- sum(placementData$Medu == i-1 & placementData$paid == "yes")
  count_momEducation_no <- sum(placementData$Medu == i-1 & placementData$paid == "no")
  
  momEducation_yes[i] <- count_momEducation_yes
  momEducation_no[i] <- count_momEducation_no
}

momEducation_data <- data.frame(
  Mother_Education = momEduLevel,
  Mother_Education_yes = momEducation_yes,
  Mother_Education_no = momEducation_no
)

ggplot(momEducation_data, aes(x = Mother_Education)) +
  geom_line(aes(y = Mother_Education_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Mother_Education_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Mother_Education_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Mother_Education_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
  scale_color_manual(
    values = c("Paid Classes" = "#FF9F1C", "Without Paid Classes" = "#2EC4B6"), 
    name = "Lines", 
    guide = guide_legend(override.aes = list(shape = NA))
  ) +
  scale_shape_manual(
    values = c("Paid Classes" = 19, "Without Paid Classes" = 19), 
    name = "Count Plots", 
    guide = guide_legend(override.aes = list(color = c("#FF9F1C", "#2EC4B6")))
  ) +
  
  labs(title = "Students' Mother Education to Paid Classes", x = "Mother Education", y = "Count")



# - Father education



# - Mother current job



# - Father current job



# - Family support



# - Internet access



# - Address



# - Curricular activities


