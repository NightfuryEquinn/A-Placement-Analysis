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
library(stringr)



# Read .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)



# Clean rows with NA values (No placement, no salary)
newPlacementData <- na.omit(placementData)



placementData[is.na(placementData)] <- 0

for (col in c("Medu", "Fedu")) {
  placementData[[col]][placementData[[col]] == 0] <- "No Education"
  placementData[[col]][placementData[[col]] == 1] <- "Primary Education"
  placementData[[col]][placementData[[col]] == 2] <- "Secondary Education"
  placementData[[col]][placementData[[col]] == 3] <- "Degree Level"
  placementData[[col]][placementData[[col]] == 4] <- "Post Graduate"
}



# Question 4 - What will affect students' master? (Density Plot)
# - Mother education
df_mom_edu <- data.frame(
  student_mom_edu = as.vector(placementData$Medu),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_mom_edu, aes(x = student_master, y = after_stat(density), color = factor(student_mom_edu))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Mother's Education") +
  scale_color_discrete(name = "Mother's Education") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Father education
df_dad_edu <- data.frame(
  student_dad_edu = as.vector(placementData$Fedu),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_dad_edu, aes(x = student_master, y = after_stat(density), color = factor(student_dad_edu))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Father's Education") +
  scale_color_discrete(name = "Father's Education") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Mother current job
df_mom_job <- data.frame(
  student_mom_job = as.vector(placementData$Mjob),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_mom_job, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_mom_job)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Mother's Current Job") +
  scale_color_discrete(name = "Mother's Current Job") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Father current job
df_dad_job <- data.frame(
  student_dad_job = as.vector(placementData$Fjob),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_dad_job, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_dad_job)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Father's Current Job") +
  scale_color_discrete(name = "Father's Current Job") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Family Support
df_famsup <- data.frame(
  student_famsup = as.vector(placementData$famsup),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_famsup, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_famsup)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Family Support") +
  scale_color_discrete(name = "Family Support") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Paid class
df_paid <- data.frame(
  student_paid = as.vector(placementData$paid),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_paid, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_paid)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Paid Classes") +
  scale_color_discrete(name = "Paid Classes") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Curricular activities
df_activities <- data.frame(
  student_activities = as.vector(placementData$activities),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_activities, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_activities)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Curricular Activities") +
  scale_color_discrete(name = "Curricular Activities") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Internet Access
df_internet <- data.frame(
  student_internet = as.vector(placementData$internet),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_internet, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_internet)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Internet Access") +
  scale_color_discrete(name = "Internet Access") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Secondary Education Board
df_secondary_board <- data.frame(
  student_secondary_board = as.vector(placementData$ssc_b),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_secondary_board, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_secondary_board)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Secondary Education Board") +
  scale_color_discrete(name = "Secondary Education Board") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )



# - Higher secondary Education Board
df_higher_secondary_board <- data.frame(
  student_higher_secondary_board = as.vector(placementData$hsc_b),
  student_master = as.vector(placementData$mba_p)
)

ggplot(df_higher_secondary_board, aes(x = student_master, y = after_stat(density), color = stringr::str_to_title(factor(student_higher_secondary_board)))) +
  geom_density(alpha = 0.6) +
  labs(x = "Students' Master Grade", y = "Density", title = "Density Plot of Students' Master Grade and Higher Secondary Education Board") +
  scale_color_discrete(name = "Higher Secondary Education Board") +
  theme(
    plot.title = element_text(hjust = 0.5), 
    legend.title = element_text(size = 12, face = "bold"), 
    legend.position = "bottom"
  )