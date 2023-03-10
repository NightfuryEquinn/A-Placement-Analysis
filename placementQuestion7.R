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



# Question 4 - What will affect students' salary?
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