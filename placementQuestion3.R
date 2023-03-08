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



# Question 3 - What will affect students to have paid classes?
educationLevel <- c(
  "No education",
  "Primary education",
  "Secondary education",
  "Degree Level",
  "Post Graduate"
)



# - Mother education (F.Polygon)
df_mom_education <- data.frame(
  mom_education = as.vector(placementData$Medu),
  student_paid = as.vector(placementData$paid)
)

freq_table <- df_mom_education %>% 
  group_by(mom_education, student_paid) %>% 
  summarize(count = n())

ggplot(freq_table, aes(x = mom_education, y = count, color = student_paid)) + 
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
  dad_education = as.vector(placementData$Fedu),
  student_paid = as.vector(placementData$paid)
)

freq_table <- df_dad_education %>% 
  group_by(dad_education, student_paid) %>% 
  summarize(count = n())

ggplot(freq_table, aes(x = dad_education, y = count, color = student_paid)) + 
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
  mom_job = as.vector(placementData$Mjob),
  student_paid = as.vector(placementData$paid)
)

ggplot(df_mom_job, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(mom_job))) +
  geom_histogram(
    alpha = 0.5, 
    position = "identity", 
    stat = "count", 
    color = "black"
  ) +
  scale_fill_manual(values = c("#D3F8E2", "#E4C1F9", "#F694C1", "#EDE7B1", "#A9DEF9")) +
  labs(
    x = "Having Paid Classes", 
    y = "Frequency", 
    title = "Overlap Histogram of Student having Paid Classes by Mother's Current Job",
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



# - Family support (Histogram)
df_famsup <- data.frame(
  student_famsup = as.vector(placementData$famsup),
  student_paid = as.vector(placementData$paid)
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
  student_internet = as.vector(placementData$internet),
  student_paid = as.vector(placementData$paid)
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
  student_address = as.vector(placementData$address),
  student_paid = as.vector(placementData$paid)
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
  student_activities = as.vector(placementData$activities),
  student_paid = as.vector(placementData$paid)
)

ggplot(df_activities, aes(x = stringr::str_to_title(student_paid), fill = stringr::str_to_title(student_activities))) +
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