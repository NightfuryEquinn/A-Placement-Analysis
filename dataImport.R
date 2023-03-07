# Clean environment to ensure data are updated
rm(list = ls())

# Install relevant libraries
# install.packages(c("crayon", "dplyr", "ggplot2", "reshape2", "stringr"))
# library(crayon)
# library(dplyr)
library(ggplot2)
# library(reshape2)
# library(stringr)

# Import the .csv file (Data Import)
placementData <- read.table(
  "C:/Users/User/Documents - Local/Degree/Sem 1/PFDA/Assignment/Assignment RScripts/placement_data.csv", 
  header = TRUE, 
  sep = ","
)

# Replace R to Rural, U to Urban (Data Manipulation)
placementData$address <- gsub("R", "Rural", placementData$address)
placementData$address <- gsub("U", "Urban", placementData$address)

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

# Change the name of headers (Data pre-processing / transformation)
alteredHeaderNames <- c(
  "UID", "Gender", "Age", "Address", "Mother_Education", "Father_Education",
  "Mother_Current_Job", "Father_Current_Job", "Family_Support", "Paid Classes",
  "Curricular_Activities", "Internet_Usage", "Secondary_Grade_Percentage",
  "Secondary_Education_Board", "Higher_Secondary_Grade_Percentage",
  "Higher_Secondary_Education_Board", "Higher_Secondary_Specialism",
  "Degree_Grade_Percentage", "Degree_Specialism", "Working_Experience",
  "Employment_Test", "Working_Specialism", "Master_Grade_Percentage",
  "Placement_Status", "Salary"
)

names(placementData) <- alteredHeaderNames
names(newPlacementData) <- alteredHeaderNames

# Get summary of the filtered .csv file (Data summarisation)
summary(placementData)
summary(newPlacementData)



### Example of ggplot Line graph
## Replace R to Rural, U to Urban (Data Manipulation)
# placementData$address <- gsub("R", "Rural", placementData$address)
# placementData$address <- gsub("U", "Urban", placementData$address)
# 
# address <- table(placementData$address)
# addressName <- as.vector(names(address))
# 
# address_yes <- vector("numeric", length(addressName))
# address_no <- vector("numeric", length(addressName))
# 
# for(i in 1:length(addressName)) {
#   count_address_yes <- sum(placementData$address == addressName[i] & placementData$paid == "yes")
#   count_address_no <- sum(placementData$address == addressName[i] & placementData$paid == "no")
#   
#   address_yes[i] <- count_address_yes
#   address_no[i] <- count_address_no
# }
# 
# address_data <- data.frame(
#   Address = stringr::str_to_title(addressName),
#   Address_yes = address_yes,
#   Address_no = address_no
# )
# 
# ggplot(address_data, aes(x = Address)) +
#   geom_line(aes(y = Address_yes, color = "Paid Classes",), linewidth = 1, group = 1) +
#   geom_point(aes(y = Address_yes, shape = "Paid Classes"), color = "#FF9F1C", size = 3) +
#   
#   geom_line(aes(y = Address_no, color = "Without Paid Classes"), linewidth = 1, group = 2) +
#   geom_point(aes(y = Address_no, shape = "Without Paid Classes"), color = "#2EC4B6", size = 3) +
#   
#   scale_color_manual(
#     values = c("Paid Classes" = "#FF9F1C", "Without Paid Classes" = "#2EC4B6"), 
#     name = "Lines", 
#     guide = guide_legend(override.aes = list(shape = NA))
#   ) +
#   scale_shape_manual(
#     values = c("Paid Classes" = 19, "Without Paid Classes" = 19), 
#     name = "Count Plots", 
#     guide = guide_legend(override.aes = list(color = c("#FF9F1C", "#2EC4B6")))
#   ) +
#   
#   labs(title = "Students' Address to Paid Classes", x = "Address", y = "Count")

