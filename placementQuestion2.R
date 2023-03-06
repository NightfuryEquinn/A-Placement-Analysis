# Clean environment to ensure data are updated
rm(list = ls())
# Clean plot graphs
if(!is.null(dev.list())) dev.off()



# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2", "stringr"))
# library(crayon)
# library(dplyr)
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
mom_pass70ssc <- vector("numeric", length(educationLevel))
mom_pass70hsc <- vector("numeric", length(educationLevel))

for(i in 1:length(educationLevel)) {
  mom_count70ssc <- sum(placementData$Medu == i-1 & placementData$ssc_p >= 70)
  mom_count70hsc <- sum(placementData$Medu == i-1 & placementData$hsc_p >= 70)
  
  mom_pass70ssc[i] <- mom_count70ssc
  mom_pass70hsc[i] <- mom_count70hsc
}

mom_pass70ssc_data <- data.frame(
  Mother_Education <- educationLevel,
  Count <- mom_pass70ssc
)

ggplot(mom_pass70ssc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))

mom_pass70hsc_data <- data.frame(
  Mother_Education <- educationLevel,
  Count <- mom_pass70hsc
)

ggplot(mom_pass70hsc_data, aes(x = "", y = Count, fill = Mother_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Mother Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#AABA9E", "#A2AE5E", "#C6B89E", "#EDD892", "#FCB97D"))



# - Father education
dadEducation <- table(placementData$Fedu)

dadEduLevel <- as.vector(names(dadEducation))
# Rename table entry with education level
names(dadEduLevel) <- educationLevel
# Create empty vector
dad_pass70ssc <- vector("numeric", length(educationLevel))
dad_pass70hsc <- vector("numeric", length(educationLevel))

for(i in 1:length(educationLevel)) {
  dad_count70ssc <- sum(placementData$Fedu == i-1 & placementData$ssc_p >= 70)
  dad_count70hsc <- sum(placementData$Fedu == i-1 & placementData$hsc_p >= 70)
  
  dad_pass70ssc[i] <- dad_count70ssc
  dad_pass70hsc[i] <- dad_count70hsc
}

dad_pass70ssc_data <- data.frame(
  Father_Education <- educationLevel,
  Count <- dad_pass70ssc
)

ggplot(dad_pass70ssc_data, aes(x = "", y = Count, fill = Father_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Secondary School Grade Percentage to Father Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FABC3C", "#BA8E46", "#F19143", "#FF773D", "#F55536"))

dad_pass70hsc_data <- data.frame(
  Father_Education <- educationLevel,
  Count <- dad_pass70hsc
)

ggplot(dad_pass70hsc_data, aes(x = "", y = Count, fill = Father_Education)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Father Education Level") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#FABC3C", "#BA8E46", "#F19143", "#FF773D", "#F55536"))



# - Mother current job
momJob <- table(placementData$Mjob)

momJobName <- as.vector(names(momJob))

# Create empty vector
momjob_pass70ssc <- vector("numeric", length(momJobName))
momjob_pass70hsc <- vector("numeric", length(momJobName))

for(i in 1:length(momJobName)) {
  momjob_count70ssc <- sum(placementData$Mjob == momJobName[i] & placementData$ssc_p >= 70)
  momjob_count70hsc <- sum(placementData$Mjob == momJobName[i] & placementData$hsc_p >= 70)
  
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
dadJob <- table(placementData$Fjob)

dadJobName <- as.vector(names(dadJob))

# Create empty vector
dadjob_pass70ssc <- vector("numeric", length(dadJobName))
dadjob_pass70hsc <- vector("numeric", length(dadJobName))

for(i in 1:length(dadJobName)) {
  dadjob_count70ssc <- sum(placementData$Fjob == dadJobName[i] & placementData$ssc_p >= 70)
  dadjob_count70hsc <- sum(placementData$Fjob == dadJobName[i] & placementData$hsc_p >= 70)
  
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

ggplot(momjob_pass70hsc_data, aes(x = "", y = Count, fill = Father_Current_Job)) +
  geom_col(width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Count", title = "Students' Higher Secondary School Grade Percentage to Father Current Job") +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#D5DFE5", "#C9B1BD", "#B49594", "#7F9172", "#567568"))



# - Family Support
famsup_yes_pass70ssc <- sum(placementData$famsup == "yes" & placementData$ssc_p >= 70)
famsup_no_pass70ssc <- sum(placementData$famsup == "no" & placementData$ssc_p >= 70)

famsup_yes_pass70hsc <- sum(placementData$famsup == "yes" & placementData$hsc_p >= 70)
famsup_no_pass70hsc <- sum(placementData$famsup == "no" & placementData$hsc_p >= 70)

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
paid_yes_pass70ssc <- sum(placementData$paid == "yes" & placementData$ssc_p >= 70)
paid_no_pass70ssc <- sum(placementData$paid == "no" & placementData$ssc_p >= 70)

paid_yes_pass70hsc <- sum(placementData$paid == "yes" & placementData$hsc_p >= 70)
paid_no_pass70hsc <- sum(placementData$paid == "no" & placementData$hsc_p >= 70)

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
activities_yes_pass70ssc <- sum(placementData$activities == "yes" & placementData$ssc_p >= 70)
activities_no_pass70ssc <- sum(placementData$activities == "no" & placementData$ssc_p >= 70)

activities_yes_pass70hsc <- sum(placementData$activities == "yes" & placementData$hsc_p >= 70)
activities_no_pass70hsc <- sum(placementData$activities == "no" & placementData$hsc_p >= 70)

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
internet_yes_pass70ssc <- sum(placementData$internet == "yes" & placementData$ssc_p >= 70)
internet_no_pass70ssc <- sum(placementData$internet == "no" & placementData$ssc_p >= 70)

internet_yes_pass70hsc <- sum(placementData$internet == "yes" & placementData$hsc_p >= 70)
internet_no_pass70hsc <- sum(placementData$internet == "no" & placementData$hsc_p >= 70)

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
secondEduBoard <- table(placementData$ssc_b)

secondEduBoardName <- as.vector(names(secondEduBoard))

# Create empty vector
secondEduBoard_pass70ssc <- vector("numeric", length(secondEduBoardName))
secondEduBoard_pass70hsc <- vector("numeric", length(secondEduBoardName))

for(i in 1:length(secondEduBoardName)) {
  secondEduBoard_count70ssc <- sum(placementData$ssc_b == secondEduBoardName[i] & placementData$ssc_p >= 70)
  secondEduBoard_count70hsc <- sum(placementData$ssc_b == secondEduBoardName[i] & placementData$hsc_p >= 70)
  
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
higherSecondEduBoard <- table(placementData$hsc_b)

higherSecondEduBoardName <- as.vector(names(higherSecondEduBoard))

# Create empty vector
higherSecondEduBoard_pass70ssc <- vector("numeric", length(higherSecondEduBoardName))
higherSecondEduBoard_pass70hsc <- vector("numeric", length(higherSecondEduBoardName))

for(i in 1:length(higherSecondEduBoardName)) {
  higherSecondEduBoard_count70ssc <- sum(placementData$hsc_b == secondEduBoardName[i] & placementData$ssc_p >= 70)
  higherSecondEduBoard_count70hsc <- sum(placementData$hsc_b == secondEduBoardName[i] & placementData$hsc_p >= 70)
  
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