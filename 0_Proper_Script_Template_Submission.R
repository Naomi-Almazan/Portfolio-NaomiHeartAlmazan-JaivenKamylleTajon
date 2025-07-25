# Load libraries
library(ggplot2)
library(psych)

# Load dataset
data <- read.csv("D:/OneDrive/Desktop/2_Demographic_Behavioral_data_Group_006.csv", stringsAsFactors = FALSE)

# Explore the data
str(data)
head(data)
summary(data)
colnames(data)

# Select and clean necessary columns
clean_data <- data[, c("Patient.ID", "Age", "Sex", "Weight_kg", "Height_cm", "BMI", 
                       "Region", "Socioeconomic", "Education", 
                       "Physical_Activity_Hours_Week", "Smoking_Status", "Drinking_Status", 
                       "Patient_Satisfaction_Score", "Health_Literacy_Score")]

# Remove missing values
clean_data <- na.omit(clean_data)

# Rename columns
colnames(clean_data) <- c("ID", "Age", "Sex", "Weight", "Height", "BMI", 
                          "Region", "SES", "Education", "PA_Hours", 
                          "Smoking", "Drinking", "Satisfaction", "Health_Lit")

# Recode factor variables
clean_data$Sex <- factor(clean_data$Sex, labels = c("Female", "Male"))
clean_data$Region <- factor(clean_data$Region, labels = c("Urban", "Rural"))
clean_data$Smoking <- factor(clean_data$Smoking, labels = c("Non-smoker", "Occasional", "Chain-smoker"))
clean_data$Drinking <- factor(clean_data$Drinking, labels = c("Non-drinker", "Casual", "Heavy"))
clean_data$SES <- factor(clean_data$SES, labels = c("Low", "Middle", "High"))
clean_data$Education <- factor(clean_data$Education, labels = c("None", "Primary", "Secondary", "Tertiary"))

# Summarize cleaned data
summary(clean_data)
describe(clean_data[, sapply(clean_data, is.numeric)])

# Plot: Histogram of Age
ggplot(clean_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal()

# Plot: Boxplot of BMI by Sex
ggplot(clean_data, aes(x = Sex, y = BMI, fill = Sex)) +
  geom_boxplot() +
  theme_minimal()

# Plot: Scatter plot of Height vs Weight
ggplot(clean_data, aes(x = Height, y = Weight)) +
  geom_point(color = "darkblue") +
  theme_minimal()

# Plot: Bar plot of Education levels
ggplot(clean_data, aes(x = Education)) +
  geom_bar(fill = "coral") +
  theme_minimal()

# T-test: BMI by Sex
t.test(BMI ~ Sex, data = clean_data)

# Correlation matrix
numeric_vars <- clean_data[, sapply(clean_data, is.numeric)]
cor(numeric_vars, use = "complete.obs")

# Export cleaned dataset
write.csv(clean_data, "cleaned_demographic_data.csv", row.names = FALSE)
