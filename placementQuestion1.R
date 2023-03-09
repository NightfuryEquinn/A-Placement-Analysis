# Clean environment to ensure data are updated
rm(list = ls())
# Clean plot graphs
if(!is.null(dev.list())) dev.off()



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2", "stringr", "Hmisc"))
library(crayon)
library(dplyr)
library(ggplot2)
library(reshape2)
library(stringr)
library(Hmisc)



# Import the .csv file (Data Import)
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)

# Replace R to Rural, U to Urban (Data Manipulation)
placementData$address <- gsub("R", "Rural", placementData$address)
placementData$address <- gsub("U", "Urban", placementData$address)

placementData$gender <- gsub("F", "Female", placementData$gender)
placementData$gender <- gsub("M", "Male", placementData$gender)

# Replace numbering to education levels
for (col in c("Medu", "Fedu")) {
  placementData[[col]][placementData[[col]] == 0] <- "No Education"
  placementData[[col]][placementData[[col]] == 1] <- "Primary Education"
  placementData[[col]][placementData[[col]] == 2] <- "Secondary Education"
  placementData[[col]][placementData[[col]] == 3] <- "Degree Level"
  placementData[[col]][placementData[[col]] == 4] <- "Post Graduate"
}

# Remove data without salary (Data Cleaning)
newPlacementData <- na.omit(placementData)

# Check for duplicated rows (Data Cleaning)
duplicated_rows <- duplicated(placementData)

# Print the number of duplicated rows (Data Cleaning)
cat("Number of duplicated rows:", sum(duplicated_rows), "\n")

# Remove duplicated rows (Data Cleaning)
noRedData <- placementData[!duplicated_rows, ]

# Change NA values to 0 (Data Cleaning)
placementData[is.na(placementData)] <- 0

# Change the name of headers (Data Pre-processing / Transformation)
alteredHeaderNames <- c(
  "UID", "Gender", "Age", "Address", "Mother_Education", "Father_Education",
  "Mother_Current_Job", "Father_Current_Job", "Family_Support", "Paid_Classes",
  "Curricular_Activities", "Internet_Usage", "Secondary_Grade_Percentage",
  "Secondary_Education_Board", "Higher_Secondary_Grade_Percentage",
  "Higher_Secondary_Education_Board", "Higher_Secondary_Specialism",
  "Degree_Grade_Percentage", "Degree_Specialism", "Working_Experience",
  "Employment_Test", "Working_Specialism", "Master_Grade_Percentage",
  "Placement_Status", "Salary"
)

names(placementData) <- alteredHeaderNames
names(newPlacementData) <- alteredHeaderNames



# Question 1 - What will affect students to get a placement?
# - Secondary school grade percentage (If higher than 70 marks)
ssc_p_placed <- sum(
  placementData$Placement_Status == "Placed" &
  placementData$Secondary_Grade_Percentage >= 70
)
ssc_p_not_placed <- sum(
  placementData$Placement_Status == "Not Placed" & 
  placementData$Secondary_Grade_Percentage >= 70
)

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
hsc_p_placed <- sum(
  placementData$Placement_Status == "Placed" & 
  placementData$Higher_Secondary_Grade_Percentage >= 70
)
hsc_p_not_placed <- sum(
  placementData$Placement_Status == "Not Placed" & 
  placementData$Higher_Secondary_Grade_Percentage >= 70
)

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



# - Degree grade (If higher than 70 marks)
degree_p_placed <- sum(
  placementData$Placement_Status == "Placed" & 
  placementData$Degree_Grade_Percentage >= 70
)
degree_p_not_placed <- sum(
  placementData$Placement_Status == "Not Placed" & 
  placementData$Degree_Grade_Percentage >= 70
)

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



# - Master grade (If higher than 70 marks)
mba_p_placed <- sum(
  placementData$Placement_Status == "Placed" &
  placementData$Master_Grade_Percentage >= 70
)
mba_p_not_placed <- sum(
  placementData$Placement_Status == "Not Placed" & 
  placementData$Master_Grade_Percentage >= 70
)

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
workex_placed_yes <- sum(placementData$Placement_Status == "Placed" & placementData$Working_Experience == "Yes")
workex_not_placed_yes <- sum(placementData$Placement_Status == "Not Placed" & placementData$Working_Experience >= "Yes")
workex_placed_no <- sum(placementData$Placement_Status == "Placed" & placementData$Working_Experience == "No")
workex_not_placed_no <- sum(placementData$Placement_Status == "Not Placed" & placementData$Working_Experience == "No")

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
etest_p_placed <- sum(
  placementData$Placement_Status == "Placed" & 
  placementData$Employment_Test >= 80
)
etest_p_not_placed <- sum(
  placementData$Placement_Status == "Not Placed" & 
  placementData$Employment_Test >= 80
)

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
ages <- table(placementData$Age)
# Create empty vector
age_placed <- vector("numeric", length(ages))
age_not_placed <- vector("numeric", length(ages))
# Extract the age as vectors and numeric
age_vec <- as.vector(names(ages))
age_num <- as.numeric(names(ages))

for(i in 1:length(age_num)) {
  age <- age_num[i]
  
  count_age_placed <- sum(placementData$Age == age & placementData$Placement_Status == "Placed")
  count_age_not_placed <- sum(placementData$Age == age & placementData$Placement_Status == "Not Placed")
  
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
  aes(x = factor(Age_Group), y = Count, fill = Placement_Status)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Age", y = "Count", fill = "") +
  ggtitle("Students' Placement to Students' Age") +
  scale_fill_manual(values = c("#BB4430", "#7EBDC2"))



# - Family Support
famsup_data_for_placement_status <- placementData %>%
  # Group data frame by status and family support
  group_by(Placement_Status, Family_Support) %>%
  # Summarizes grouped data count
  summarise(Count = n()) %>%
  # Removes grouping info from data frame
  ungroup() %>%
  # Add a new column to the data frame called Status based on values of Status and Family Support
  # If yes, then Placed or Not Placed with Family Support
  # If no, then Placed or Not Placed without Family Support
  mutate(Status = ifelse(
    Family_Support == "yes", 
    paste0(Placement_Status, " with Family Support"), 
    paste0(Placement_Status, " without Family Support")
  )) %>%
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
  group_by(Placement_Status, Paid_Classes) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(
    Paid_Classes == "yes", 
    paste0(Placement_Status, " with Paid Class"),
    paste0(Placement_Status, " without Paid Class")
  )) %>%
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
  group_by(Placement_Status, Curricular_Activities) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(
    Curricular_Activities == "yes",
    paste0(Placement_Status, " with Curricular Activities"), 
    paste0(Placement_Status, " without Curricular Activities")
  )) %>%
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
  group_by(Placement_Status, Internet_Usage) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(Status = ifelse(
    Internet_Usage == "yes", 
    paste0(Placement_Status, " with Internet Access"), 
    paste0(Placement_Status, " without Internet Access")
  )) %>%
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