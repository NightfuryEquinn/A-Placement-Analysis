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
# library(stringr)



# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



placementData[is.na(placementData)] <- 0



# Question 4 - What will affect students' salary?
# - Secondary eduboard (Boxplot)
secondary_edu_table <- table(placementData$ssc_b)
secondary_edu_name <- as.vector(names(secondary_edu_table))

df_secondary <- data.frame(
  secondary_x = secondary_edu_name,
  secondary_y = as.vector(placementData$salary)
)

ggplot(df_secondary, aes(x = secondary_x, y = secondary_y)) +
  geom_boxplot(fill = "#3B905A") +
  geom_jitter(aes(color = "Salary"), width = 0.2, alpha = 0.5, size = 3) +
  labs(
    x = "Secondary Education Board", 
    y = "Count", 
    title = "Boxplot of Secondary Education Board and Students' Salary",
    color = "Plots"
  ) +
  scale_color_manual(values = c("#95E06C")) +
  theme(legend.title = element_text(face = "bold", size = 12))



# - Secondary grade (Density Plot)
df_secondary_grade <- data.frame(
  student_secondary_grade = as.vector(placementData$ssc_p),
  student_salary = as.vector(placementData$salary)
)

ggplot(df_secondary_grade, aes(x = student_secondary_grade, y = ..density.., color = factor(student_salary))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Secondary Grade", y = "Density", title = "Density Plot of Students' Secondary Grade and Salary") +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Higher secondary eduboard (Boxplot)
higher_secondary_edu_table <- table(placementData$hsc_b)
higher_secondary_edu_name <- as.vector(names(higher_secondary_edu_table))

df_higher_secondary <- data.frame(
  higher_secondary_x = higher_secondary_edu_name,
  higher_secondary_y = as.vector(placementData$salary)
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
  student_higher_secondary_grade = as.vector(placementData$hsc_p),
  student_salary = as.vector(placementData$salary)
)

ggplot(df_higher_secondary_grade, aes(x = student_higher_secondary_grade, y = ..density.., color = factor(student_salary))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Higher Secondary Grade", y = "Density", title = "Density Plot of Students' Higher Secondary Grade and Salary") +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )

# - Higher secondary specialisation (Countplot)
# - Degree grade (Density Plot)
df_degree_grade <- data.frame(
  student_degree_grade = as.vector(placementData$degree_p),
  student_salary = as.vector(placementData$salary)
)

ggplot(df_degree_grade, aes(x = student_degree_grade, y = ..density.., color = factor(student_salary))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Degree Grade", y = "Density", title = "Density Plot of Students' Degree and Salary") +
  scale_color_discrete(name = "Students' Salary") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )


# - Degree specialisation (Countplot)
# - Master grade (Scatterplot)
# - Work experiences (Violin Plot)
# - Employment test (Scatterplot)
# - Employment specialisation (Countplot)