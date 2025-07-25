# LOAD DATA
data <- read.csv("D:/OneDrive/Desktop/Dataset_even (2).csv", stringsAsFactors = FALSE)

# Inspect structure
str(data)
head(data)
summary(data)
colnames(data)

#LOAD PACKAGES 
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(readr)

# Reload data (already done above, technically redundant)
data <- read.csv("D:/OneDrive/Desktop/Dataset_even (2).csv", stringsAsFactors = FALSE)

#  CLEANING AND FORMATTING 
# Drop unneeded legend columns
data <- data %>%
  select(-contains("Legend"), -Medication)

# Rename columns for clarity
colnames(data) <- c("PatientID", "Age", "Sex", "Weight_kg", "Height_cm", "BMI",
                    "Systolic_BP", "Diastolic_BP", "Hypertension", "HeartRate",
                    "SmokingStatus", "PhysicalActivity_Hrs", "StressLevel",
                    "SleepHours", "Glucose", "Cholesterol", "Risk")

# Convert data types
data$Sex <- factor(data$Sex, labels = c("Female", "Male"))
data$SmokingStatus <- factor(data$SmokingStatus,
                             levels = c(0, 1, 2),
                             labels = c("Non-Smoker", "Occasional", "Chainsmoker"))
data$Hypertension <- factor(data$Hypertension)
data$Risk <- factor(data$Risk)

# Handle missing values
data <- data %>% drop_na()

# DESCRIPTIVE STATISTICS 
summary_stats <- describe(data %>% select_if(is.numeric))
print(summary_stats)

# BMI CATEGORIZATION
data$BMI_Category <- cut(data$BMI,
                         breaks = c(0, 18.5, 24.9, 29.9, Inf),
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))

# VISUALIZATIONS 
# Histogram of BMI
ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "BMI Distribution", x = "BMI", y = "Count")

# Boxplot of Glucose by Smoking Status
ggplot(data, aes(x = SmokingStatus, y = Glucose, fill = SmokingStatus)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Glucose Levels by Smoking Status")

# Scatterplot: BMI vs Systolic BP
ggplot(data, aes(x = BMI, y = Systolic_BP)) +
  geom_point(alpha = 0.6, color = "purple") +
  theme_minimal() +
  labs(title = "BMI vs Systolic Blood Pressure")

# Scatterplot: Weight vs Physical Activity Hours
ggplot(data, aes(x = Weight_kg, y = PhysicalActivity_Hrs)) +
  geom_point(color = "darkgreen") +
  theme_minimal() +
  labs(title = "Weight vs Physical Activity")

# ADVANCED ANALYSIS 
# Correlation: Glucose and Cholesterol
cor_test <- cor.test(data$Glucose, data$Cholesterol)
print(cor_test)

# Chi-Square test: BMI Category vs Smoking Status
chi_table <- table(data$BMI_Category, data$SmokingStatus)
chi_test <- chisq.test(chi_table)
print(chi_test)
