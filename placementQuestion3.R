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



# - Mother education
momEducation <- table(placementData$Medu)
momEduLevel <- as.vector(names(momEducation))

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

ggplot(momEducation_data, aes(x = educationLevel)) +
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
  
  labs(title = "Students' Mother Education to Paid Classes", x = "Mother Education", y = "Count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))


# - Father education
dadEducation <- table(placementData$Fedu)
dadEduLevel <- as.vector(names(dadEducation))

dadEducation_yes <- vector("numeric", length(educationLevel))
dadEducation_no <- vector("numeric", length(educationLevel))

for(i in 1:length(educationLevel)) {
  count_dadEducation_yes <- sum(placementData$Fedu == i-1 & placementData$paid == "yes")
  count_dadEducation_no <- sum(placementData$Fedu == i-1 & placementData$paid == "no")
  
  dadEducation_yes[i] <- count_dadEducation_yes
  dadEducation_no[i] <- count_dadEducation_no
}

dadEducation_data <- data.frame(
  Father_Education = dadEduLevel,
  Father_Education_yes = dadEducation_yes,
  Father_Education_no = dadEducation_no
)

ggplot(dadEducation_data, aes(x = educationLevel)) +
  geom_line(aes(y = Father_Education_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Father_Education_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Father_Education_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Father_Education_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Father Education to Paid Classes", x = "Father Education", y = "Count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Mother current job
momJob <- table(placementData$Mjob)
momJobName <- as.vector(names(momJob))

momJob_yes <- vector("numeric", length(momJobName))
momJob_no <- vector("numeric", length(momJobName))

for(i in 1:length(momJobName)) {
  count_momJob_yes <- sum(placementData$Mjob == momJobName[i] & placementData$paid == "yes")
  count_momJob_no <- sum(placementData$Mjob == momJobName[i] & placementData$paid == "no")
  
  momJob_yes[i] <- count_momJob_yes
  momJob_no[i] <- count_momJob_no
}

momJob_data <- data.frame(
  Mother_Current_Job = stringr::str_to_title(momJobName),
  Mother_Current_Job_yes = momJob_yes,
  Mother_Current_Job_no = momJob_no
)

ggplot(momJob_data, aes(x = Mother_Current_Job)) +
  geom_line(aes(y = Mother_Current_Job_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Mother_Current_Job_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Mother_Current_Job_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Mother_Current_Job_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Mother Current Job to Paid Classes", x = "Mother Current Job", y = "Count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Father current job
dadJob <- table(placementData$Fjob)
dadJobName <- as.vector(names(dadJob))

dadJob_yes <- vector("numeric", length(dadJobName))
dadJob_no <- vector("numeric", length(dadJobName))

for(i in 1:length(dadJobName)) {
  count_dadJob_yes <- sum(placementData$Fjob == dadJobName[i] & placementData$paid == "yes")
  count_dadJob_no <- sum(placementData$Fjob == dadJobName[i] & placementData$paid == "no")
  
  dadJob_yes[i] <- count_dadJob_yes
  dadJob_no[i] <- count_dadJob_no
}

dadJob_data <- data.frame(
  Father_Current_Job = stringr::str_to_title(dadJobName),
  Father_Current_Job_yes = dadJob_yes,
  Father_Current_Job_no = dadJob_no
)

ggplot(dadJob_data, aes(x = Father_Current_Job)) +
  geom_line(aes(y = Father_Current_Job_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Father_Current_Job_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Father_Current_Job_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Father_Current_Job_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Father Current Job to Paid Classes", x = "Father Current Job", y = "Count") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Family support
famsup <- table(placementData$famsup)
famsupName <- as.vector(names(famsup))

famsup_yes <- vector("numeric", length(famsupName))
famsup_no <- vector("numeric", length(famsupName))

for(i in 1:length(famsupName)) {
  count_famsup_yes <- sum(placementData$famsup == famsupName[i] & placementData$paid == "yes")
  count_famsup_no <- sum(placementData$famsup == famsupName[i] & placementData$paid == "no")
  
  famsup_yes[i] <- count_famsup_yes
  famsup_no[i] <- count_famsup_no
}

famsup_data <- data.frame(
  Family_Support_Status = stringr::str_to_title(famsupName),
  Family_Support_Status_yes = famsup_yes,
  Family_Support_Status_no = famsup_no
)

ggplot(famsup_data, aes(x = Family_Support_Status)) +
  geom_line(aes(y = Family_Support_Status_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Family_Support_Status_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Family_Support_Status_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Family_Support_Status_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Family Support to Paid Classes", x = "Family Support", y = "Count")



# - Internet access
internet <- table(placementData$internet)
internetName <- as.vector(names(internet))

internet_yes <- vector("numeric", length(internetName))
internet_no <- vector("numeric", length(internetName))

for(i in 1:length(internetName)) {
  count_internet_yes <- sum(placementData$internet == internetName[i] & placementData$paid == "yes")
  count_internet_no <- sum(placementData$internet == internetName[i] & placementData$paid == "no")
  
  internet_yes[i] <- count_internet_yes
  internet_no[i] <- count_internet_no
}

internet_data <- data.frame(
  Internet_Status = stringr::str_to_title(internetName),
  Internet_Status_yes = internet_yes,
  Internet_Status_no = internet_no
)

ggplot(internet_data, aes(x = Internet_Status)) +
  geom_line(aes(y = Internet_Status_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Internet_Status_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Internet_Status_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Internet_Status_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Internet Access to Paid Classes", x = "Internet Access", y = "Count")



# - Address
# Replace R to Rural, U to Urban (Data Manipulation)
placementData$address <- gsub("R", "Rural", placementData$address)
placementData$address <- gsub("U", "Urban", placementData$address)

address <- table(placementData$address)
addressName <- as.vector(names(address))

address_yes <- vector("numeric", length(addressName))
address_no <- vector("numeric", length(addressName))

for(i in 1:length(addressName)) {
  count_address_yes <- sum(placementData$address == addressName[i] & placementData$paid == "yes")
  count_address_no <- sum(placementData$address == addressName[i] & placementData$paid == "no")
  
  address_yes[i] <- count_address_yes
  address_no[i] <- count_address_no
}

address_data <- data.frame(
  Address = stringr::str_to_title(addressName),
  Address_yes = address_yes,
  Address_no = address_no
)

ggplot(address_data, aes(x = Address)) +
  geom_line(aes(y = Address_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Address_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Address_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Address_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Address to Paid Classes", x = "Address", y = "Count")



# - Curricular activities
activities <- table(placementData$activities)
activitiesName <- as.vector(names(activities))

activities_yes <- vector("numeric", length(activitiesName))
activities_no <- vector("numeric", length(activitiesName))

for(i in 1:length(activitiesName)) {
  count_activities_yes <- sum(placementData$activities == activitiesName[i] & placementData$paid == "yes")
  count_activities_no <- sum(placementData$activities == activitiesName[i] & placementData$paid == "no")
  
  activities_yes[i] <- count_activities_yes
  activities_no[i] <- count_activities_no
}

activities_data <- data.frame(
  Activities = stringr::str_to_title(activitiesName),
  Activities_yes = activities_yes,
  Activities_no = activities_no
)

ggplot(activities_data, aes(x = Activities)) +
  geom_line(aes(y = Activities_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
  geom_point(aes(y = Activities_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
  
  geom_line(aes(y = Activities_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
  geom_point(aes(y = Activities_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
  
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
  
  labs(title = "Students' Curricular Activities to Paid Classes", x = "Curricular Activities", y = "Count")