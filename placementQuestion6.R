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

# # Replace numbering to education levels
# for (col in c("Medu", "Fedu")) {
#   placementData[[col]][placementData[[col]] == 0] <- "No Education"
#   placementData[[col]][placementData[[col]] == 1] <- "Primary Education"
#   placementData[[col]][placementData[[col]] == 2] <- "Secondary Education"
#   placementData[[col]][placementData[[col]] == 3] <- "Degree Level"
#   placementData[[col]][placementData[[col]] == 4] <- "Post Graduate"
# }

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