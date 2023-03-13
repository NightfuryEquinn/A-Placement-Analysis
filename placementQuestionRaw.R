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

# Get summary of the filtered .csv file (Data exploration)
summary(placementData)
summary(newPlacementData)

names(placementData)
names(newPlacementData)

nrow(placementData)
nrow(newPlacementData)

ncol(placementData)
ncol(newPlacementData)

str(placementData)
str(newPlacementData)

head(placementData)
head(newPlacementData)

tail(placementData)
tail(newPlacementData)


# Use line graph and count plot to generate some random raw analysis about the dataset
# - Age (Count Plot)
df_age <- data.frame(
  studentAge = as.vector(placementData$Age)
)

ggplot(df_age, aes(x = studentAge, fill = factor(studentAge))) +
  geom_bar() +
  scale_fill_manual(values = c(
    "#7400B8", "#5E60CE", "#4EA8DE", "#56CFE1", "#72EFDD", "#52B788"
  )) +
  labs(
    x = "Student Age",
    y = "Count",
    title = "Count Plot of Student Age",
    fill = "Age Category"
  )



# - Address (Count Plot)
df_address <- data.frame(
  studentAddress = as.vector(placementData$Address)
)

ggplot(df_address, aes(x = studentAddress, fill = factor(studentAddress))) +
  geom_bar() +
  scale_fill_manual(values = c("#4E148C", "#858AE3")) +
  labs(
    x = "Student Address",
    y = "Count",
    title = "Count Plot of Student Address",
    fill = "Address Category"
  )



# - Gender (Count Plot)
df_gender <- data.frame(
  studentGender = as.vector(placementData$Gender)
)

ggplot(df_gender, aes(x = studentGender, fill = factor(studentGender))) +
  geom_bar() +
  scale_fill_manual(values = c("#1A5B92", "#0899BA")) +
  labs(
    x = "Student Gender",
    y = "Count",
    title = "Count Plot of Student Gender",
    fill = "Gender Category"
  )



# - Salary (Count Plot)
df_salary <- data.frame(
  studentSalary = as.vector(placementData$Salary)
)

ggplot(df_salary, aes(x = studentSalary, fill = factor(studentSalary))) +
  geom_bar() +
  scale_fill_manual(
    values = c(
      "#001219", "#005F73",
      "#0A9396", "#94D2BD",
      "#E9D8A6", "#EE9B00",
      "#CA6702", "#BB3E03",
      "#AE2012", "#9B2226",
      "#03045E", "#0077B6"
    )
  ) +
  labs(
    x = "Student Salary",
    y = "Count",
    title = "Count Plot of Student Salary",
    fill = "Salary Category"
  )



# - Placement (Count Plot)
df_placement <- data.frame(
  studentPlacement = as.vector(placementData$Placement_Status)
)

ggplot(df_placement, aes(x = studentPlacement, fill = factor(studentPlacement))) +
  geom_bar() +
  scale_fill_manual(values = c("#134611", "#3E8914")) +
  labs(
    x = "Student Placement",
    y = "Count",
    title = "Count Plot of Student Placement",
    fill = "Placement Status"
  )