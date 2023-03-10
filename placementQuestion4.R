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