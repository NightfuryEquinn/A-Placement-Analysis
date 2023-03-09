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