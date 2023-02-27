# Clean environment to ensure data are updated
rm(list = ls())



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2"))
# library(crayon)
# library(dplyr)
library(ggplot2)



# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



# Question 1 - What will affect students to get a placement?
# - Secondary school grade percentage (If higher than 70 marks)
ssc_p_placed <- sum(placementData$status == "Placed" & placementData$ssc_p >= 70)
ssc_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$ssc_p >= 70)

data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(ssc_p_placed, ssc_p_not_placed)
)

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Secondary School Grade Percentage") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Higher secondary school grade percentage (If higher than 70 marks)
hsc_p_placed <- sum(placementData$status == "Placed" & placementData$hsc_p >= 70)
hsc_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$hsc_p >= 70)

data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(hsc_p_placed, hsc_p_not_placed)
)

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Higher Secondary School Grade Percentage") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Degree grade
degree_p_placed <- sum(placementData$status == "Placed" & placementData$degree_p >= 70)
degree_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$degree_p >= 70)

data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(degree_p_placed, degree_p_not_placed)
)

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Degree Grade Percentage") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Master grade
mba_p_placed <- sum(placementData$status == "Placed" & placementData$mba_p >= 70)
mba_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$mba_p >= 70)

data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(mba_p_placed, mba_p_not_placed)
)

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Master Grade Percentage") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Working experience
workex_placed_yes <- sum(placementData$status == "Placed" & placementData$workex == "Yes")
workex_not_placed_yes <- sum(placementData$status == "Not Placed" & placementData$workex >= "Yes")
workex_placed_no <- sum(placementData$status == "Placed" & placementData$workex == "No")
workex_not_placed_no <- sum(placementData$status == "Not Placed" & placementData$workex == "No")

data_for_placement_status <- data.frame(
  Status = c(
    "Placed with Working Experience", 
    "Not Placed with Working Experience", 
    "Placed without Working Experience",
    "Not Placed without Working Experience"
  ),
  Count = c(
    workex_placed_yes,
    workex_not_placed_yes, 
    workex_placed_no, 
    workex_not_placed_no)
  )

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Working Experience") +
  scale_fill_manual(values = c("#DB2B39", "#29335C", "#F3A712", "#534D41")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Employment test (If higher than 80 marks)
etest_p_placed <- sum(placementData$status == "Placed" & placementData$etest_p >= 80)
etest_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$etest_p >= 80)

data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(etest_p_placed, etest_p_not_placed)
)

ggplot(data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Employment Test") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Age


# - Family Support


# - Paid class


# - Curricular activities


# - Internet Access

