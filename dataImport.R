# Clean environment to ensure data are updated
rm(list = ls())

# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2"))
# library(crayon)
# library(dplyr)
library(ggplot2)

# Import the .csv file
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)

# Remove data without salary
newPlacementData <- na.omit(placementData)

# Get summary of the .csv file
summary(newPlacementData)

# Pie Chart
countGender <- table(newPlacementData$gender)

pie(countGender, labels = c("Female", "Male"), col = c("#ED0101", "#0C44AC"))

# Bar Chart
countAddress <- table(newPlacementData$address)

barplot(countAddress, xlab = "Address", ylab = "Count", ylim = c(0, 5000), main = "Address Distribution", col = c("#CB997E", "#6B705C"))

# Scatter Chart
filteredSSC <- subset(placementData, select = c("ssc_p", "status"))

ggplot(data = filteredSSC, aes(x = ssc_p, y = status)) + 
  geom_point(aes(shape = status, color = status)) + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Secondary School Grade", y = "Job Placement") + 
  scale_shape_manual(values = c(1, 16)) +
  scale_color_manual(values = c("red", "green")) + 
  theme(legend.position = "bottom") + 
  theme_minimal()

# Line Chart
countAge <- table(newPlacementData$age)

plot(countAge, type = "o", main = "Age Distribution", xlab = "Age", ylab = "Count", col = "blue")