# Clean environment to ensure data are updated
rm(list = ls())



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2", "stringr"))
# library(crayon)
# library(dplyr)
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



# Question 1 - What will affect students to get a placement?
# - Secondary school grade percentage (If higher than 70 marks)
ssc_p_placed <- sum(placementData$status == "Placed" & placementData$ssc_p >= 70)
ssc_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$ssc_p >= 70)

ssc_data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(ssc_p_placed, ssc_p_not_placed)
)

ggplot(ssc_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
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

hsc_data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(hsc_p_placed, hsc_p_not_placed)
)

ggplot(hsc_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
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

degree_data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(degree_p_placed, degree_p_not_placed)
)

ggplot(degree_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
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

mba_data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(mba_p_placed, mba_p_not_placed)
)

ggplot(mba_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
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

workex_data_for_placement_status <- data.frame(
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

ggplot(workex_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Working Experience") +
  scale_fill_manual(values = c("#7B904B", "#58641D", "#273B09", "#002400")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Employment test (If higher than 80 marks)
etest_p_placed <- sum(placementData$status == "Placed" & placementData$etest_p >= 80)
etest_p_not_placed <- sum(placementData$status == "Not Placed" & placementData$etest_p >= 80)

etest_data_for_placement_status <- data.frame(
  Status = c("Placed", "Not Placed"),
  Count = c(etest_p_placed, etest_p_not_placed)
)

ggplot(etest_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Placement Status") +
  ylab("Count") +
  ggtitle("Students' Placement to Students' Employment Test") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Age
ages <- table(placementData$age)
# Create empty vector
age_placed <- vector("numeric", length(ages))
age_not_placed <- vector("numeric", length(ages))
# Extract the age as vectors and numeric
age_vec <- as.vector(names(ages))
age_num <- as.numeric(names(ages))

for(i in 1:length(age_num)) {
  age <- age_num[i]
  
  count_age_placed <- sum(placementData$age == age & placementData$status == "Placed")
  count_age_not_placed <- sum(placementData$age == age & placementData$status == "Not Placed")
  
  age_placed[i] <- count_age_placed
  age_not_placed[i] <-count_age_not_placed
}

age_data_for_placement_status <- data.frame(
  Age_Group = age_vec,
  Placed = age_placed,
  Not_Placed = age_not_placed
)

age_data_for_placement_status_long <- melt(
  age_data_for_placement_status,
  id.vars = "Age_Group",
  variable.name = "Placement_Status", 
  value.name = "Count"
  )

ggplot(
  age_data_for_placement_status_long, 
  aes(
    x = factor(Age_Group), 
    y = Count, 
    fill = Placement_Status
    )) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Age", y = "Count", fill = "") +
  ggtitle("Students' Placement to Students' Age") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Family Support
famsup_data_for_placement_status <- placementData %>%
  # Group data frame by status and family support
  group_by(status, famsup) %>%
  # Summarises grouped data count
  summarise(Count = n()) %>%
  # Removes grouping info from data frame
  ungroup() %>%
  # Add a new column to the data frame called Status based on values of Status and Family Support
  # If yes, then Placed or Not Placed with Family Support
  # If no, then Placed or Not Placed without Family Support
  mutate(Status = ifelse(famsup == "yes", paste0(status, " with Family Support"), paste0(status, " without Family Support"))) %>%
  # Select both columns from data frame
  select(Status, Count)

ggplot(famsup_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Family Support") +
  ylab("Count") +
  ggtitle("Students' Placement to Family Support") +
  scale_fill_manual(values = c("#B5B682", "#7C9885", "#28666E", "#033F63")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Paid class
paid_data_for_placement_status <- placementData %>%
  group_by(status, paid) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(paid == "yes", paste0(status, " with Paid Class"), paste0(status, " without Paid Class"))) %>%
  select(Status, Count)

ggplot(paid_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Paid Class") +
  ylab("Count") +
  ggtitle("Students' Placement to Paid Class") +
  scale_fill_manual(values = c("#A9927D", "#5E503F", "#22333B", "#0A0908")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Curricular activities
activities_data_for_placement_status <- placementData %>%
  group_by(status, activities) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(activities == "yes", paste0(status, " with Curricular Activities"), paste0(status, " without Curricular Activities"))) %>%
  select(Status, Count)

ggplot(activities_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Curricular Activities") +
  ylab("Count") +
  ggtitle("Students' Placement to Curricular Activities") +
  scale_fill_manual(values = c("#4CC9F0", "#4361EE", "#7209B7", "#3A0CA3")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



# - Internet Access
internet_data_for_placement_status <- placementData %>%
  group_by(status, internet) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(internet == "yes", paste0(status, " with Internet Access"), paste0(status, " without Internet Access"))) %>%
  select(Status, Count)

ggplot(internet_data_for_placement_status, aes(x = Status, y = Count, fill = Status)) +
  # Height of bars based on data values
  # Separate each bars for better reading
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Internet Access") +
  ylab("Count") +
  ggtitle("Students' Placement to Internet Access") +
  scale_fill_manual(values = c("#E6AF2E", "#A3320B", "#6B0504", "#001514")) +
  # Rotate x-axis label, align to the right
  theme(axis.text.x = element_text(angle = -45, hjust = 0))