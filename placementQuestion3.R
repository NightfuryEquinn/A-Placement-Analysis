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



# Question 3 - What will affect students' degree? (Boxplot)
# - Mother education
df_mom_edu <- data.frame(
  mom_edu_x = as.vector(placementData$Medu),
  mom_edu_y = as.vector(placementData$degree_p)
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
  dad_edu_x = as.vector(placementData$Fedu),
  dad_edu_y = as.vector(placementData$degree_p)
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
  mom_job_x = as.vector(placementData$Mjob),
  mom_job_y = as.vector(placementData$degree_p)
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
  dad_job_x = as.vector(placementData$Fjob),
  dad_job_y = as.vector(placementData$degree_p)
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
  famsup_x = as.vector(placementData$famsup),
  famsup_y = as.vector(placementData$degree_p)
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
  paid_x = as.vector(placementData$paid),
  paid_y = as.vector(placementData$degree_p)
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
  activities_x = as.vector(placementData$activities),
  activities_y = as.vector(placementData$degree_p)
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
  internet_x = as.vector(placementData$internet),
  internet_y = as.vector(placementData$degree_p)
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
  secondary_board_x = as.vector(placementData$ssc_b),
  secondary_board_y = as.vector(placementData$degree_p)
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
  higher_secondary_board_x = as.vector(placementData$hsc_b),
  higher_secondary_board_y = as.vector(placementData$degree_p)
)

ggplot(df_higher_secondary_board, aes(x = stringr::str_to_title(factor(higher_secondary_board_x)), y = higher_secondary_board_y)) +
  geom_boxplot(fill = c("#E9C46A", "#F4A261", "#E76F51")) +
  labs(
    x = "Higher Secondary Education Board", 
    y = "Grade", 
    title = "Boxplot of Higher Secondary Education Board and Students' Degree Grade"
  ) 