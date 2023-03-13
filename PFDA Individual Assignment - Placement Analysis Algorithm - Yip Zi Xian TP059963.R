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

# Get summary of the filtered .csv file (Data exploration)
summary(placementData)
summary(newPlacementData)

names(placementData)
names(newPlacementData)

nrow(placementData)
nrow(newPlacementData)

ncol(placementData)
ncol(newPlacementData)

str(placementData)
str(newPlacementData)

head(placementData)
head(newPlacementData)

tail(placementData)
tail(newPlacementData)


# Use line graph and count plot to generate some random raw analysis about the dataset
# - Age (Count Plot)
df_age <- data.frame(
  studentAge = as.vector(placementData$Age)
)

ggplot(df_age, aes(x = studentAge, fill = factor(studentAge))) +
  geom_bar() +
  scale_fill_manual(values = c(
    "#7400B8", "#5E60CE", "#4EA8DE", "#56CFE1", "#72EFDD", "#52B788"
  )) +
  labs(
    x = "Student Age",
    y = "Count",
    title = "Count Plot of Student Age",
    fill = "Age Category"
  )



# - Address (Count Plot)
df_address <- data.frame(
  studentAddress = as.vector(placementData$Address)
)

ggplot(df_address, aes(x = studentAddress, fill = factor(studentAddress))) +
  geom_bar() +
  scale_fill_manual(values = c("#4E148C", "#858AE3")) +
  labs(
    x = "Student Address",
    y = "Count",
    title = "Count Plot of Student Address",
    fill = "Address Category"
  )



# - Gender (Count Plot)
df_gender <- data.frame(
  studentGender = as.vector(placementData$Gender)
)

ggplot(df_gender, aes(x = studentGender, fill = factor(studentGender))) +
  geom_bar() +
  scale_fill_manual(values = c("#1A5B92", "#0899BA")) +
  labs(
    x = "Student Gender",
    y = "Count",
    title = "Count Plot of Student Gender",
    fill = "Gender Category"
  )



# - Salary (Count Plot)
df_salary <- data.frame(
  studentSalary = as.vector(placementData$Salary)
)

ggplot(df_salary, aes(x = studentSalary, fill = factor(studentSalary))) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "#001219", "#005F73",
      "#0A9396", "#94D2BD",
      "#E9D8A6", "#EE9B00",
      "#CA6702", "#BB3E03",
      "#AE2012", "#9B2226",
      "#03045E", "#0077B6"
    )
  ) +
  labs(
    x = "Student Salary",
    y = "Count",
    title = "Count Plot of Student Salary",
    fill = "Salary Category"
  )



# - Placement (Count Plot)
df_placement <- data.frame(
  studentPlacement = as.vector(placementData$Placement_Status)
)

ggplot(df_placement, aes(x = studentPlacement, fill = factor(studentPlacement))) +
  geom_bar() +
  scale_fill_manual(values = c("#134611", "#3E8914")) +
  labs(
    x = "Student Placement",
    y = "Count",
    title = "Count Plot of Student Placement",
    fill = "Placement Status"
  )



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



# Question 2 - What will affect students to get grades higher than 70 marks in secondary school and higher secondary school?
# - Mother education
momEducation <- table(placementData$Mother_Education)

momEduLevel <- as.vector(names(momEducation))
# Create empty vector
mom_pass70ssc <- vector("numeric", length(momEduLevel))
mom_pass70hsc <- vector("numeric", length(momEduLevel))

for(i in 1:length(momEduLevel)) {
  mom_count70ssc <- sum(placementData$Mother_Education == momEduLevel[i] & placementData$Secondary_Grade_Percentage >= 70)
  mom_count70hsc <- sum(placementData$Mother_Education == momEduLevel[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  mom_pass70ssc[i] <- mom_count70ssc
  mom_pass70hsc[i] <- mom_count70hsc
}

mom_pass70ssc_data <- data.frame(
  Mother_Education <- momEduLevel,
  Count <- mom_pass70ssc
)

ggplot(mom_pass70ssc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))

mom_pass70hsc_data <- data.frame(
  Mother_Education <- momEduLevel,
  Count <- mom_pass70hsc
)

ggplot(mom_pass70hsc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))



# - Father education
dadEducation <- table(placementData$Father_Education)

dadEduLevel <- as.vector(names(dadEducation))
# Create empty vector
dad_pass70ssc <- vector("numeric", length(dadEduLevel))
dad_pass70hsc <- vector("numeric", length(dadEduLevel))

for(i in 1:length(dadEduLevel)) {
  dad_count70ssc <- sum(placementData$Father_Education == dadEduLevel[i] & placementData$Secondary_Grade_Percentage >= 70)
  dad_count70hsc <- sum(placementData$Father_Education == dadEduLevel[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  dad_pass70ssc[i] <- dad_count70ssc
  dad_pass70hsc[i] <- dad_count70hsc
}

dad_pass70ssc_data <- data.frame(
  Father_Education <- dadEduLevel,
  Count <- dad_pass70ssc
)

ggplot(dad_pass70ssc_data, aes(x = "", y = Count, fill = Father_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Father Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FABC3C", "#BA8E46", "#F19143", "#FF773D", "#F55536"))

dad_pass70hsc_data <- data.frame(
  Father_Education <- dadEduLevel,
  Count <- dad_pass70hsc
)

ggplot(dad_pass70hsc_data, aes(x = "", y = Count, fill = Father_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Father Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FABC3C", "#BA8E46", "#F19143", "#FF773D", "#F55536"))



# - Mother current job
momJob <- table(placementData$Mother_Current_Job)

momJobName <- as.vector(names(momJob))

# Create empty vector
momjob_pass70ssc <- vector("numeric", length(momJobName))
momjob_pass70hsc <- vector("numeric", length(momJobName))

for(i in 1:length(momJobName)) {
  momjob_count70ssc <- sum(placementData$Mother_Current_Job == momJobName[i] & placementData$Secondary_Grade_Percentage >= 70)
  momjob_count70hsc <- sum(placementData$Mother_Current_Job == momJobName[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  momjob_pass70ssc[i] <- momjob_count70ssc
  momjob_pass70hsc[i] <- momjob_count70hsc
}

momjob_pass70ssc_data <- data.frame(
  Mother_Current_Job <- str_to_title(momJobName),
  Count <- momjob_pass70ssc
)

ggplot(momjob_pass70ssc_data, aes(x = "", y = Count, fill = Mother_Current_Job)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Mother Current Job") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#BEE9E8", "#62B6CB", "#5C8DB0", "#5FA8D3", "#1B4965"))

momjob_pass70hsc_data <- data.frame(
  Mother_Current_Job <- str_to_title(momJobName),
  Count <- momjob_pass70hsc
)

ggplot(momjob_pass70hsc_data, aes(x = "", y = Count, fill = Mother_Current_Job)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Mother Current Job") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#BEE9E8", "#62B6CB", "#5C8DB0", "#5FA8D3", "#1B4965"))



# - Father current job
dadJob <- table(placementData$Father_Current_Job)

dadJobName <- as.vector(names(dadJob))

# Create empty vector
dadjob_pass70ssc <- vector("numeric", length(dadJobName))
dadjob_pass70hsc <- vector("numeric", length(dadJobName))

for(i in 1:length(dadJobName)) {
  dadjob_count70ssc <- sum(placementData$Father_Current_Job == dadJobName[i] & placementData$Secondary_Grade_Percentage >= 70)
  dadjob_count70hsc <- sum(placementData$Father_Current_Job == dadJobName[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  dadjob_pass70ssc[i] <- dadjob_count70ssc
  dadjob_pass70hsc[i] <- dadjob_count70hsc
}

dadjob_pass70ssc_data <- data.frame(
  Father_Current_Job <- str_to_title(dadJobName),
  Count <- dadjob_pass70ssc
)

ggplot(dadjob_pass70ssc_data, aes(x = "", y = Count, fill = Father_Current_Job)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Father Current Job") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#D5DFE5", "#C9B1BD", "#B49594", "#7F9172", "#567568"))

dadjob_pass70hsc_data <- data.frame(
  Father_Current_Job <- str_to_title(dadJobName),
  Count <- dadjob_pass70hsc
)

ggplot(dadjob_pass70hsc_data, aes(x = "", y = Count, fill = Father_Current_Job)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Father Current Job") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#D5DFE5", "#C9B1BD", "#B49594", "#7F9172", "#567568"))



# - Family Support
famsup_yes_pass70ssc <- sum(placementData$Family_Support == "yes" & placementData$Secondary_Grade_Percentage >= 70)
famsup_no_pass70ssc <- sum(placementData$Family_Support == "no" & placementData$Secondary_Grade_Percentage >= 70)

famsup_yes_pass70hsc <- sum(placementData$Family_Support == "yes" & placementData$Higher_Secondary_Grade_Percentage >= 70)
famsup_no_pass70hsc <- sum(placementData$Family_Support == "no" & placementData$Higher_Secondary_Grade_Percentage >= 70)

famsup_pass70ssc <- data.frame(
  Status <- c("Family Support", "Without Family Support"),
  Count <- c(famsup_yes_pass70ssc, famsup_no_pass70ssc)
)

ggplot(famsup_pass70ssc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Family Support") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A873E8", "#5D7BD5"))

famsup_pass70hsc <- data.frame(
  Status <- c("Family Support", "Without Family Support"),
  Count <- c(famsup_yes_pass70hsc, famsup_no_pass70hsc)
)

ggplot(famsup_pass70hsc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Family Support") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#A873E8", "#5D7BD5"))



# - Paid class
paid_yes_pass70ssc <- sum(placementData$Paid_Classes == "yes" & placementData$Secondary_Grade_Percentage >= 70)
paid_no_pass70ssc <- sum(placementData$Paid_Classes == "no" & placementData$Secondary_Grade_Percentage >= 70)

paid_yes_pass70hsc <- sum(placementData$Paid_Classes == "yes" & placementData$Higher_Secondary_Grade_Percentage >= 70)
paid_no_pass70hsc <- sum(placementData$Paid_Classes == "no" & placementData$Higher_Secondary_Grade_Percentage >= 70)

paid_pass70ssc <- data.frame(
  Status <- c("Paid Classes", "Without Paid Classes"),
  Count <- c(paid_yes_pass70ssc, paid_no_pass70ssc)
)

ggplot(paid_pass70ssc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Paid Classes") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FFBA49", "#20A39E"))

paid_pass70hsc <- data.frame(
  Status <- c("Paid Classes", "Without Paid Classes"),
  Count <- c(paid_yes_pass70hsc, paid_no_pass70hsc)
)

ggplot(paid_pass70hsc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Paid Classes") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FFBA49", "#20A39E"))



# - Curricular activities
activities_yes_pass70ssc <- sum(placementData$Curricular_Activities == "yes" & placementData$Secondary_Grade_Percentage >= 70)
activities_no_pass70ssc <- sum(placementData$Curricular_Activities == "no" & placementData$Secondary_Grade_Percentage >= 70)

activities_yes_pass70hsc <- sum(placementData$Curricular_Activities == "yes" & placementData$Higher_Secondary_Grade_Percentage >= 70)
activities_no_pass70hsc <- sum(placementData$Curricular_Activities == "no" & placementData$Higher_Secondary_Grade_Percentage >= 70)

activities_pass70ssc <- data.frame(
  Status <- c("Curricular Activities", "Without Curricular Activities"),
  Count <- c(activities_yes_pass70ssc, activities_no_pass70ssc)
)

ggplot(activities_pass70ssc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Curricular Activites") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#09814A", "#E5C687"))

activities_pass70hsc <- data.frame(
  Status <- c("Curricular Activities", "Without Curricular Activites"),
  Count <- c(activities_yes_pass70hsc, activities_no_pass70hsc)
)

ggplot(activities_pass70hsc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Curricular Activities") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#09814A", "#E5C687"))



# - Internet Access
internet_yes_pass70ssc <- sum(placementData$Internet_Usage == "yes" & placementData$Secondary_Grade_Percentage >= 70)
internet_no_pass70ssc <- sum(placementData$Internet_Usage == "no" & placementData$Secondary_Grade_Percentage >= 70)

internet_yes_pass70hsc <- sum(placementData$Internet_Usage == "yes" & placementData$Higher_Secondary_Grade_Percentage >= 70)
internet_no_pass70hsc <- sum(placementData$Internet_Usage == "no" & placementData$Higher_Secondary_Grade_Percentage >= 70)

internet_pass70ssc <- data.frame(
  Status <- c("Internet Access", "Without Internet Access"),
  Count <- c(internet_yes_pass70ssc, internet_no_pass70ssc)
)

ggplot(internet_pass70ssc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Internet Access") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#EABE7C", "#DD6031"))

internet_pass70hsc <- data.frame(
  Status <- c("Curricular Activities", "Without Curricular Activites"),
  Count <- c(internet_yes_pass70hsc, internet_no_pass70hsc)
)

ggplot(internet_pass70hsc, aes(x = "", y = Count, fill = Status)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Internet Access") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#EABE7C", "#DD6031"))



# - Secondary education board
secondEduBoard <- table(placementData$Secondary_Education_Board)

secondEduBoardName <- as.vector(names(secondEduBoard))

# Create empty vector
secondEduBoard_pass70ssc <- vector("numeric", length(secondEduBoardName))
secondEduBoard_pass70hsc <- vector("numeric", length(secondEduBoardName))

for(i in 1:length(secondEduBoardName)) {
  secondEduBoard_count70ssc <- sum(placementData$Secondary_Education_Board == secondEduBoardName[i] & placementData$Secondary_Grade_Percentage >= 70)
  secondEduBoard_count70hsc <- sum(placementData$Secondary_Education_Board == secondEduBoardName[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  secondEduBoard_pass70ssc[i] <- secondEduBoard_count70ssc
  secondEduBoard_pass70hsc[i] <- secondEduBoard_count70hsc
}

secondEduBoard_pass70ssc_data <- data.frame(
  Secondary_Education_Board <- secondEduBoardName,
  Count <- secondEduBoard_pass70ssc
)

ggplot(secondEduBoard_pass70ssc_data, aes(x = "", y = Count, fill = Secondary_Education_Board)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Secondary Education Board") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#E6AF2E", "#BEB7A4", "#3D348D"))

secondEduBoard_pass70hsc_data <- data.frame(
  Secondary_Education_Board <- secondEduBoardName,
  Count <- secondEduBoard_pass70hsc
)

ggplot(secondEduBoard_pass70hsc_data, aes(x = "", y = Count, fill = Secondary_Education_Board)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Secondary Education Board") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#E6AF2E", "#BEB7A4", "#3D348D"))



# - Higher secondary education board
higherSecondEduBoard <- table(placementData$Higher_Secondary_Education_Board)

higherSecondEduBoardName <- as.vector(names(higherSecondEduBoard))

# Create empty vector
higherSecondEduBoard_pass70ssc <- vector("numeric", length(higherSecondEduBoardName))
higherSecondEduBoard_pass70hsc <- vector("numeric", length(higherSecondEduBoardName))

for(i in 1:length(higherSecondEduBoardName)) {
  higherSecondEduBoard_count70ssc <- sum(placementData$Higher_Secondary_Education_Board == secondEduBoardName[i] & placementData$Secondary_Grade_Percentage >= 70)
  higherSecondEduBoard_count70hsc <- sum(placementData$Higher_Secondary_Education_Board == secondEduBoardName[i] & placementData$Higher_Secondary_Grade_Percentage >= 70)
  
  higherSecondEduBoard_pass70ssc[i] <- higherSecondEduBoard_count70ssc
  higherSecondEduBoard_pass70hsc[i] <- higherSecondEduBoard_count70hsc
}

higherSecondEduBoard_pass70ssc_data <- data.frame(
  Higher_Secondary_Education_Board <- higherSecondEduBoardName,
  Count <- higherSecondEduBoard_pass70ssc
)

ggplot(higherSecondEduBoard_pass70ssc_data, aes(x = "", y = Count, fill = Higher_Secondary_Education_Board)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Students' Higher Secondary Education Board") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#6A9588", "#7CA982", "#C2A83E"))

higherSecondEduBoard_pass70hsc_data <- data.frame(
  Higher_Secondary_Education_Board <- higherSecondEduBoardName,
  Count <- higherSecondEduBoard_pass70hsc
)

ggplot(higherSecondEduBoard_pass70hsc_data, aes(x = "", y = Count, fill = Higher_Secondary_Education_Board)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Students' Higher Secondary Education Board") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#6A9588", "#7CA982", "#C2A83E"))



# Question 3 - What will affect students' degree? (Boxplot)
# - Mother education
df_mom_edu <- data.frame(
  mom_edu_x = as.vector(placementData$Mother_Education),
  mom_edu_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_mom_edu, aes(x = factor(mom_edu_x), y = mom_edu_y)) +
  geom_boxplot(fill = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Mother's Education", 
    y = "Grade", 
    title = "Boxplot of Mother's Education and Students' Degree Grade"
  ) 



# - Father education
df_dad_edu <- data.frame(
  dad_edu_x = as.vector(placementData$Father_Education),
  dad_edu_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_dad_edu, aes(x = factor(dad_edu_x), y = dad_edu_y)) +
  geom_boxplot(fill = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Father's Education", 
    y = "Grade", 
    title = "Boxplot of Father's Education and Students' Degree Grade"
  )



# - Mother current job
df_mom_job <- data.frame(
  mom_job_x = as.vector(placementData$Mother_Current_Job),
  mom_job_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_mom_job, aes(x = stringr::str_to_title(factor(mom_job_x)), y = mom_job_y)) +
  geom_boxplot(fill = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Mother's Current Job", 
    y = "Grade", 
    title = "Boxplot of Mother's Current Job and Students' Degree Grade"
  ) 



# - Father current job
df_dad_job <- data.frame(
  dad_job_x = as.vector(placementData$Father_Current_Job),
  dad_job_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_dad_job, aes(x = stringr::str_to_title(factor(dad_job_x)), y = dad_job_y)) +
  geom_boxplot(fill = c("#264653", "#2A9D8F", "#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Father's Current Job", 
    y = "Grade", 
    title = "Boxplot of Father's Current Job and Students' Degree Grade"
  ) 



# - Family Support
df_famsup <- data.frame(
  famsup_x = as.vector(placementData$Family_Support),
  famsup_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_famsup, aes(x = stringr::str_to_title(factor(famsup_x)), y = famsup_y)) +
  geom_boxplot(fill = c("#2A9D8F", "#E9C46A")) +
  labs(
    x = "Family Support", 
    y = "Grade", 
    title = "Boxplot of Family Support and Students' Degree Grade"
  ) 



# - Paid class
df_paid <- data.frame(
  paid_x = as.vector(placementData$Paid_Classes),
  paid_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_paid, aes(x = stringr::str_to_title(factor(paid_x)), y = paid_y)) +
  geom_boxplot(fill = c("#2A9D8F", "#E9C46A")) +
  labs(
    x = "Paid Classes", 
    y = "Grade", 
    title = "Boxplot of Paid Classes and Students' Degree Grade"
  ) 



# - Curricular activities
df_activities <- data.frame(
  activities_x = as.vector(placementData$Curricular_Activities),
  activities_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_activities, aes(x = stringr::str_to_title(factor(activities_x)), y = activities_y)) +
  geom_boxplot(fill = c("#2A9D8F", "#E9C46A")) +
  labs(
    x = "Curricular Activities", 
    y = "Grade", 
    title = "Boxplot of Curricular Activities and Students' Degree Grade"
  ) 


# - Internet Access
df_internet <- data.frame(
  internet_x = as.vector(placementData$Internet_Usage),
  internet_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_internet, aes(x = stringr::str_to_title(factor(internet_x)), y = internet_y)) +
  geom_boxplot(fill = c("#2A9D8F", "#E9C46A")) +
  labs(
    x = "Internet Access", 
    y = "Grade", 
    title = "Boxplot of Internet Access and Students' Degree Grade"
  ) 



# - Secondary Education Board
df_secondary_board <- data.frame(
  secondary_board_x = as.vector(placementData$Secondary_Education_Board),
  secondary_board_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(df_secondary_board, aes(x = stringr::str_to_title(factor(secondary_board_x)), y = secondary_board_y)) +
  geom_boxplot(fill = c("#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Secondary Education Board", 
    y = "Grade", 
    title = "Boxplot of Secondary Education Board and Students' Degree Grade"
  ) 



# - Higher secondary Education Board
df_higher_secondary_board <- data.frame(
  higher_secondary_board_x = as.vector(placementData$Higher_Secondary_Education_Board),
  higher_secondary_board_y = as.vector(placementData$Degree_Grade_Percentage)
)

ggplot(
  df_higher_secondary_board, 
  aes(x = stringr::str_to_title(factor(higher_secondary_board_x)), y = higher_secondary_board_y)
) +
  geom_boxplot(fill = c("#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Higher Secondary Education Board", 
    y = "Grade", 
    title = "Boxplot of Higher Secondary Education Board and Students' Degree Grade"
  ) 



# Question 4 - What will affect students' master? (Density Plot)
# - Mother education
df_mom_edu <- data.frame(
  student_mom_edu = as.vector(placementData$Mother_Education),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(df_mom_edu, aes(x = student_master, y = after_stat(density), color = factor(student_mom_edu))) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Mother's Education"
  ) +
  scale_color_discrete(name = "Mother's Education") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Father education
df_dad_edu <- data.frame(
  student_dad_edu = as.vector(placementData$Father_Education),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(df_dad_edu, aes(x = student_master, y = after_stat(density), color = factor(student_dad_edu))) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Father's Education"
  ) +
  scale_color_discrete(name = "Father's Education") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Mother current job
df_mom_job <- data.frame(
  student_mom_job = as.vector(placementData$Mother_Current_Job),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_mom_job, 
  aes(
    x = student_master, 
    y = after_stat(density), 
    color = stringr::str_to_title(factor(student_mom_job))
  )) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Mother's Current Job"
  ) +
  scale_color_discrete(name = "Mother's Current Job") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Father current job
df_dad_job <- data.frame(
  student_dad_job = as.vector(placementData$Father_Current_Job),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_dad_job, 
  aes(
    x = student_master, y = after_stat(density), 
    color = stringr::str_to_title(factor(student_dad_job))
  )) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Father's Current Job"
  ) +
  scale_color_discrete(name = "Father's Current Job") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Family Support
df_famsup <- data.frame(
  student_famsup = as.vector(placementData$Family_Support),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_famsup, 
  aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_famsup)))
) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Family Support"
  ) +
  scale_color_discrete(name = "Family Support") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Paid class
df_paid <- data.frame(
  student_paid = as.vector(placementData$Paid_Classes),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_paid, 
  aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_paid)))
) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Paid Classes"
  ) +
  scale_color_discrete(name = "Paid Classes") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Curricular activities
df_activities <- data.frame(
  student_activities = as.vector(placementData$Curricular_Activities),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_activities, 
  aes(
    x = student_master, y = after_stat(density), 
    color = stringr::str_to_title(factor(student_activities))
  )) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Curricular Activities"
  ) +
  scale_color_discrete(name = "Curricular Activities") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Internet Access
df_internet <- data.frame(
  student_internet = as.vector(placementData$Internet_Usage),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_internet, 
  aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_internet)))
) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Internet Access"
  ) +
  scale_color_discrete(name = "Internet Access") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Secondary Education Board
df_secondary_board <- data.frame(
  student_secondary_board = as.vector(placementData$Secondary_Education_Board),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_secondary_board, 
  aes(
    x = student_master, y = after_stat(density), 
    color = stringr::str_to_title(factor(student_secondary_board))
  )) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Secondary Education Board"
  ) +
  scale_color_discrete(name = "Secondary Education Board") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Higher secondary Education Board
df_higher_secondary_board <- data.frame(
  student_higher_secondary_board = as.vector(placementData$Higher_Secondary_Education_Board),
  student_master = as.vector(placementData$Master_Grade_Percentage)
)

ggplot(
  df_higher_secondary_board, 
  aes(
    x = student_master, y = after_stat(density), 
    color = stringr::str_to_title(factor(student_higher_secondary_board))
  )) +
  geom_density(alpha = 0.6) +
  labs(
    x = "Students' Master Grade", y = "Density", 
    title = "Density Plot of Students' Master Grade and Higher Secondary Education Board"
  ) +
  scale_color_discrete(name = "Higher Secondary Education Board") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# Question 5 - What will affect students' employment test? (Violin Plot)
# - Mother education
df_mom_edu <- data.frame(
  student_mom_edu = as.vector(placementData$Mother_Education),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(df_mom_edu, aes(x = factor(student_mom_edu), y = student_employ_test, color = factor(student_mom_edu))) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#F7B538", "#DB7C26", "#D8572A", "#C32F27", "#780116")) +
  labs(
    x = "Mother's Education", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Mother's Education and Students' Employment Test",
    color = "Education Level"
  ) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Father education
df_dad_edu <- data.frame(
  student_dad_edu = as.vector(placementData$Father_Education),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(df_dad_edu, aes(x = factor(student_dad_edu), y = student_employ_test, color = factor(student_dad_edu))) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#F7B538", "#DB7C26", "#D8572A", "#C32F27", "#780116")) +
  labs(
    x = "Father's Education", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Father's Education and Students' Employment Test",
    color = "Education Level"
  ) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Mother current job
df_mom_job <- data.frame(
  student_mom_job = as.vector(placementData$Mother_Current_Job),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_mom_job, 
  aes(
    x = stringr::str_to_title(factor(student_mom_job)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_mom_job))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#F7B538", "#DB7C26", "#D8572A", "#C32F27", "#780116")) +
  labs(
    x = "Mother's Current Job", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Mother's Current Job and Students' Employment Test",
    color = "Job"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Father current job
df_dad_job <- data.frame(
  student_dad_job = as.vector(placementData$Father_Current_Job),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_dad_job, 
  aes(
    x = stringr::str_to_title(factor(student_dad_job)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_dad_job))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#F7B538", "#DB7C26", "#D8572A", "#C32F27", "#780116")) +
  labs(
    x = "Father's Current Job", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Father's Current Job and Students' Employment Test",
    color = "Job"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Family Support
df_famsup <- data.frame(
  student_famsup = as.vector(placementData$Family_Support),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_famsup, 
  aes(
    x = stringr::str_to_title(factor(student_famsup)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_famsup))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#60712F", "#274029")) +
  labs(
    x = "Family Support", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Family Support and Students' Employment Test",
    color = "Family Support"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Paid class
df_paid <- data.frame(
  student_paid = as.vector(placementData$Paid_Classes),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_paid, 
  aes(
    x = stringr::str_to_title(factor(student_paid)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_paid))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#60712F", "#274029")) +
  labs(
    x = "Paid Classes", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Paid Classes and Students' Employment Test",
    color = "Paid Classes"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Curricular activities
df_activities <- data.frame(
  student_activities = as.vector(placementData$Curricular_Activities),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_activities, 
  aes(
    x = stringr::str_to_title(factor(student_activities)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_activities))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#60712F", "#274029")) +
  labs(
    x = "Curricular Activities", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Curricular Activities and Students' Employment Test",
    color = "Curricular Activities"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Internet Access
df_internet <- data.frame(
  student_internet = as.vector(placementData$Internet_Usage),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_internet, 
  aes(
    x = stringr::str_to_title(factor(student_internet)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_internet))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#60712F", "#274029")) +
  labs(
    x = "Internet Access", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Internet Access and Students' Employment Test",
    color = "Internet Access"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Secondary Education Board
df_secondary_board <- data.frame(
  student_secondary_board = as.vector(placementData$Secondary_Education_Board),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_secondary_board, 
  aes(
    x = stringr::str_to_title(factor(student_secondary_board)), y = student_employ_test,
    color = stringr::str_to_title(factor(student_secondary_board))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#1282A2", "#005CB3", "#002D7A")) +
  labs(
    x = "Secondary Education Board", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Secondary Education Board and Students' Employment Test",
    color = "Secondary Education Board"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# - Higher secondary Education Board
df_higher_secondary_board <- data.frame(
  student_higher_secondary_board = as.vector(placementData$Higher_Secondary_Education_Board),
  student_employ_test = as.vector(placementData$Employment_Test)
)

ggplot(
  df_higher_secondary_board, 
  aes(
    x = stringr::str_to_title(factor(student_higher_secondary_board)), y = student_employ_test, 
    color = stringr::str_to_title(factor(student_higher_secondary_board))
  )) +
  geom_violin(trim = FALSE, alpha = 0.6, linewidth = 1.2) +
  scale_color_manual(values = c("#1282A2", "#005CB3", "#002D7A")) +
  labs(
    x = "Higher Secondary Education Board", 
    y = "Students' Employment Test", 
    title = "Violin Plot of Higher Secondary Education Board and Students' Employment Test",
    color = "Higher Secondary Education Board"
  ) +
  stat_summary(fun.data = mean_sdl, geom = "errorbar", width = 0.2) +
  stat_summary(fun = "median", geom = "point", shape = 21, size = 3, fill = "white") +
  stat_summary(fun.data = median_hilow, geom = "crossbar", width = 0.5, fill = NA, color = "black")



# Question 6 - What will affect students to have paid classes?
# - Mother education (F.Polygon)
df_mom_education <- data.frame(
  mom_education = as.vector(placementData$Mother_Education),
  student_paid = as.vector(placementData$Paid_Classes)
)

freq_table <- df_mom_education %>% 
  group_by(mom_education, student_paid) %>% 
  summarise(count = n())

ggplot(freq_table, aes(x = mom_education, y = count, color = stringr::str_to_title(student_paid))) + 
  geom_line(stat = "identity") + 
  geom_point() + 
  labs(
    title = "Frequency Polygon of Mother's Education by Paid Classes",
    x = "Mother's Education Level",
    y = "Count",
    color = "Paid Classes"
  )



# - Father education (F.Polygon)
df_dad_education <- data.frame(
  dad_education = as.vector(placementData$Father_Education),
  student_paid = as.vector(placementData$Paid_Classes)
)

freq_table <- df_dad_education %>% 
  group_by(dad_education, student_paid) %>% 
  summarise(count = n())

ggplot(freq_table, aes(x = dad_education, y = count, color = stringr::str_to_title(student_paid))) + 
  geom_line(stat = "identity") + 
  geom_point() + 
  labs(
    title = "Frequency Polygon of Father's Education by Paid Classes",
    x = "Father's Education Level",
    y = "Count",
    color = "Paid Classes"
  )



# - Mother current job (Histogram)
df_mom_job <- data.frame(
  mom_job = as.vector(placementData$Mother_Current_Job),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(df_mom_job, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(mom_job))) +
  geom_histogram(
    alpha = 0.6, 
    position = "dodge", 
    stat = "count", 
    color = "black"
  ) +
  scale_fill_manual(values = c("#A4FFC9", "#E4C1F9", "#F694C1", "#FFF27E", "#A9DEF9")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Histogram of Student having Paid Classes by Mother's Current Job",
    fill = "Family Support"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# - Father current job (Histogram)
df_dad_job <- data.frame(
  dad_job = as.vector(placementData$Father_Current_Job),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(df_dad_job, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(dad_job))) +
  geom_histogram(
    alpha = 0.6, 
    position = "dodge", 
    stat = "count", 
    color = "black"
  ) +
  scale_fill_manual(values = c("#A4FFC9", "#E4C1F9", "#F694C1", "#FFF27E", "#A9DEF9")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Histogram of Student having Paid Classes by Father's Current Job",
    fill = "Family Support"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# - Family support (Histogram)
df_famsup <- data.frame(
  student_famsup = as.vector(placementData$Family_Support),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(df_famsup, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(student_famsup))) +
  geom_histogram(
    alpha = 0.5, 
    position = "identity", 
    stat = "count", 
    color = "black"
  ) +
  scale_fill_manual(values = c("#970005", "#000052")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Overlap Histogram of Student having Paid Classes by Family Support",
    fill = "Family Support"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# - Internet access (Histogram)
df_internet <- data.frame(
  student_internet = as.vector(placementData$Internet_Usage),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(df_internet, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(student_internet))) +
  geom_histogram(alpha = 0.5, position = "identity", stat = "count", color = "black") +
  scale_fill_manual(values = c("#970005", "#000052")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Overlap Histogram of Student having Paid Classes by Internet Access",
    fill = "Internet Access"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# - Address (Histogram)
df_address <- data.frame(
  student_address = as.vector(placementData$Address),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(df_address, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(student_address))) +
  geom_histogram(alpha = 0.5, position = "identity", stat = "count", color = "black") +
  scale_fill_manual(values = c("#970005", "#000052")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Overlap Histogram of Student having Paid Classes by Address",
    fill = "Address"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# - Curricular activities (Histogram)
df_activities <- data.frame(
  student_activities = as.vector(placementData$Curricular_Activities),
  student_paid = as.vector(placementData$Paid_Classes)
)

ggplot(
  df_activities, 
  aes(
    x = stringr::str_to_title(student_paid), 
    fill = stringr::str_to_title(student_activities)
  )) +
  geom_histogram(alpha = 0.5, position = "identity", stat = "count", color = "black") +
  scale_fill_manual(values = c("#970005", "#000052")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Overlap Histogram of Student having Paid Classes by Curricular Activities",
    fill = "Curricular Activities"
  ) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.9),
    vjust = 0.5,
    size = 4
  )



# Question 7 - What will affect students' salary?
# - Secondary eduboard (Boxplot)
secondary_edu_table <- table(placementData$Secondary_Education_Board)
secondary_edu_name <- as.vector(names(secondary_edu_table))

df_secondary <- data.frame(
  secondary_x = secondary_edu_name,
  secondary_y = as.vector(placementData$Salary)
)

ggplot(df_secondary, aes(x = secondary_x, y = secondary_y)) +
  geom_boxplot(fill = "#3B905A") +
  geom_jitter(aes(color = "Salary"), width = 0.2, alpha = 0.5, size = 3) +
  labs(
    x = "Secondary Education Board", 
    y = "Salary", 
    title = "Boxplot of Secondary Education Board and Students' Salary",
    color = "Plots"
  ) +
  scale_color_manual(values = c("#95E06C")) +
  theme(legend.title = element_text(face = "bold", size = 12))



# - Secondary grade (Density Plot)
df_secondary_grade <- data.frame(
  student_secondary_grade = as.vector(placementData$Secondary_Grade_Percentage),
  student_salary = as.vector(placementData$Salary)
)

ggplot(
  df_secondary_grade, 
  aes(x = student_secondary_grade, y = after_stat(density), color = factor(student_salary))
) +
  geom_density(alpha = 0.6, linewidth = 1) +
  labs(
    x = "Students' Secondary Grade", y = "Density", 
    title = "Density Plot of Students' Secondary Grade and Salary"
  ) +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Higher secondary eduboard (Boxplot)
higher_secondary_edu_table <- table(placementData$Higher_Secondary_Education_Board)
higher_secondary_edu_name <- as.vector(names(higher_secondary_edu_table))

df_higher_secondary <- data.frame(
  higher_secondary_x = higher_secondary_edu_name,
  higher_secondary_y = as.vector(placementData$Salary)
)

ggplot(df_higher_secondary, aes(x = higher_secondary_x, y = higher_secondary_y)) +
  geom_boxplot(fill = "#3B905A") +
  geom_jitter(aes(color = "Salary"), width = 0.2, alpha = 0.5, size = 3) +
  labs(
    x = "Higher Secondary Education Board",
    y = "Count", 
    title = "Boxplot of Higher Secondary Education Board and Students' Salary", 
    color = "Plots"
  ) +
  scale_color_manual(values = c("#95E06C")) +
  theme(legend.title = element_text(face = "bold", size = 12))



# - Higher secondary grade (Density Plot)
df_higher_secondary_grade <- data.frame(
  student_higher_secondary_grade = as.vector(placementData$Higher_Secondary_Grade_Percentage),
  student_salary = as.vector(placementData$Salary)
)

ggplot(
  df_higher_secondary_grade,
  aes(x = student_higher_secondary_grade, y = after_stat(density), color = factor(student_salary))
) +
  geom_density(alpha = 0.6, linewidth = 1) +
  labs(
    x = "Students' Higher Secondary Grade", y = "Density",
    title = "Density Plot of Students' Higher Secondary Grade and Salary"
  ) +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Higher secondary specialisation (Violin Plot)
df_higher_secondary_specialisation <- data.frame(
  student_higher_secondary_specialisation = as.vector(placementData$Higher_Secondary_Specialism),
  student_salary = as.vector(placementData$Salary)
)

ggplot(
  df_higher_secondary_specialisation, 
  aes(x = factor(student_higher_secondary_specialisation), y = student_salary)
) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "#110B11", linewidth = 1.2) +
  geom_jitter(aes(color = student_higher_secondary_specialisation), width = 0.2, alpha = 0.7) +
  labs(
    x = "Students' Higher Secondary Specialisation", 
    y = "Students' Salary", 
    title = "Violin Plot of Students' Higher Secondary Specialisation and Salary",
    color = "Higher Secondary Specialisation"
  ) +
  scale_color_manual(values = c("#B7990D", "#8CADA7", "#A5D0A8")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )



# - Degree grade (Density Plot)
df_degree_grade <- data.frame(
  student_degree_grade = as.vector(placementData$Degree_Grade_Percentage),
  student_salary = as.vector(placementData$Salary)
)

ggplot(df_degree_grade, aes(x = student_degree_grade, y = after_stat(density), color = factor(student_salary))) +
  geom_density(alpha = 0.6, linewidth = 1) +
  labs(x = "Students' Degree Grade", y = "Density", title = "Density Plot of Students' Degree and Salary") +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Degree specialization (Violin Plot)
df_degree_specialisation <- data.frame(
  student_degree_specialisation = as.vector(placementData$Degree_Specialism),
  student_salary = as.vector(placementData$Salary)
)

ggplot(df_degree_specialisation, aes(x = factor(student_degree_specialisation), y = student_salary)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "#F1BF98", linewidth = 1.2) +
  geom_jitter(aes(color = student_degree_specialisation), width = 0.2, alpha = 0.7) +
  labs(
    x = "Students' Degree Specialisation", 
    y = "Students' Salary", 
    title = "Violin Plot of Students' Degree Specialisation and Salary",
    color = "Degree Specialisation"
  ) +
  scale_color_manual(values = c("#6B818C", "#CC2936", "#08415C")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )



# - Master grade (Scatterplot)
df_master <- data.frame(
  student_mba = c(placementData$Master_Grade_Percentage),
  student_salary = c(placementData$Salary),
  # Create a categorical variable based on the salary
  salary_category = cut(placementData$Salary, breaks = c(0, 300000, Inf), labels = c("Category A", "Category B"))
)

# Create the plot
ggplot(df_master, aes(x = student_mba, y = student_salary, color = salary_category)) +
  geom_point(size = 2, shape = 16) +
  geom_smooth(aes(group = salary_category), method = "lm", se = FALSE, color = "#D81159") +
  scale_color_manual(values = c("#FFBC42", "#73D2DE"), 
                     name = "Salary Category", 
                     labels = c("<= 300k", "> 300k", "No Salary"), 
                     drop = FALSE,
                     na.value = "#218380") +
  labs(title = "Scatter Plot of Students' Master Grade to Students' Salary", x = "MBA Percentage", y = "Salary")



# - Work experiences (Histogram)
df_workex <- data.frame(
  student_workex = as.vector(placementData$Working_Experience),
  student_salary = as.vector(placementData$Salary)
)
# Create an overlap histogram
ggplot(df_workex, aes(x = student_salary, fill = student_workex)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20, color = "black") +
  scale_fill_manual(values = c("#970005", "#000052")) +
  labs(
    x = "Salary", 
    y = "Frequency", 
    title = "Overlap Histogram of Student Salary by Working Experience",
    fill = "Working Experience"
  )



# - Employment test (Scatterplot)
df_employtest <- data.frame(
  student_employtest = c(placementData$Employment_Test),
  student_salary = c(placementData$Salary),
  salary_category = cut(placementData$Salary, breaks = c(0, 300000, Inf), labels = c("Category A", "Category B"))
)

ggplot(df_employtest, aes(x = student_employtest, y = student_salary, color = salary_category)) +
  geom_point(size = 2, shape = 16) +
  geom_smooth(aes(group = salary_category), method = "lm", se = FALSE, color = "#0A236F") +
  scale_color_manual(values = c("#C5B32D", "#3DA170"), 
                     name = "Salary Category", 
                     labels = c("<= 300k", "> 300k", "No Salary"), 
                     drop = FALSE,
                     na.value = "#5D737E") +
  labs(title = "Scatter Plot of Students' Employment Test to Students' Salary", x = "Employment Test", y = "Salary")



# - Employment specialization (Violin Plot)
df_employ_specialisation <- data.frame(
  student_employ_specialisation = as.vector(placementData$Working_Specialism),
  student_salary = as.vector(placementData$Salary)
)

ggplot(df_employ_specialisation, aes(x = factor(student_employ_specialisation), y = student_salary)) +
  geom_violin(trim = FALSE, alpha = 0.6, color = "#607196", linewidth = 1.2) +
  geom_jitter(aes(color = student_employ_specialisation), width = 0.2, alpha = 0.7) +
  labs(
    x = "Students' Employment Specialisation", 
    y = "Students' Salary", 
    title = "Violin Plot of Students' Employment Specialisation and Salary",
    color = "Employment Specialisation"
  ) +
  scale_color_manual(values = c("#FFC759", "#FF7B9C")) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )