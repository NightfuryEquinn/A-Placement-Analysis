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
df <- subset(newPlacementData, select = c("etest_p", "mba_p", "specialisation"))

df$etest_p <- as.numeric(df$etest_p)
df$mba_p <- as.numeric(df$mba_p)

ggplot(data = df, aes(x = etest_p, y = mba_p)) + 
  geom_point(aes(shape = specialisation, color = specialisation)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_shape_manual(values=c(3, 16)) +
  scale_color_manual(values=c('#999999','#E69F00')) +
  theme(legend.position="top")

# Line Chart
countAge <- table(newPlacementData$age)

plot(countAge, type = "o", main = "Age Distribution", xlab = "Age", ylab = "Count", col = "blue")