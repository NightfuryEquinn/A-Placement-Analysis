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