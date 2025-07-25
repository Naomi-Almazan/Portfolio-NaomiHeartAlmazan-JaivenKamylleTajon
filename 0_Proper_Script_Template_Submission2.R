# Load Required Libraries

library(ggplot2)
library(psych)
library(dplyr)
library(tidyverse)


# Load and Inspect Data

data <- read.csv("D:/OneDrive/Desktop/3_Nutritional_Dietary_data_Group_006.csv", stringsAsFactors = FALSE)
str(data)
head(data)
summary(data)
colnames(data)


# Clean Column Names

colnames(data) <- tolower(gsub("\\.", "_", colnames(data)))


# Handle Missing Values

sum(is.na(data))  # Check total missing values
data <- na.omit(data)  # Remove missing rows

long_data <- data %>%
  pivot_longer(cols = c(protein_intake_g, fat_intake_g, carbohydrate_intake_g),
               names_to = "Nutrient",
               values_to = "Intake")


# Descriptive Statistics

summary_stats <- data %>%
  select(-patient_id) %>%
  summarise_all(list(
    mean = ~mean(.),
    median = ~median(.),
    sd = ~sd(.),
    min = ~min(.),
    max = ~max(.)
  ))
print(summary_stats)


# Categorize BMI and Activity

data$bmi_category <- cut(data$bmi,
                         breaks = c(-Inf, 18.5, 25, 30, Inf),
                         labels = c("Underweight", "Normal", "Overweight", "Obese"))

data$activity_level <- cut(data$physical_activity_hours_week,
                           breaks = c(-Inf, 1, 3, 5, Inf),
                           labels = c("Sedentary", "Light", "Moderate", "Active"))


# Chi-Square Test: BMI vs Activity Level

chisq_result <- chisq.test(table(data$bmi_category, data$activity_level))
print(chisq_result)

# Interpretation
if (chisq_result$p.value < 0.05) {
  cat("\n✅ Significant relationship between BMI category and activity level.\n")
} else {
  cat("\nℹ️ No significant relationship between BMI category and activity level.\n")
}


# Bar Plot: BMI Category vs Activity Level

ggplot(data, aes(x = bmi_category, fill = activity_level)) +
  geom_bar(position = "dodge") +
  labs(title = "BMI Categories vs Activity Levels",
       x = "BMI Category",
       y = "Count") +
  theme_minimal()


# Histograms

ggplot(data, aes(x = body_fat_percent)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of Body Fat %")

ggplot(data, aes(x = muscle_mass_kg)) + 
  geom_histogram(binwidth = 2, fill = "salmon", color = "black") + 
  labs(title = "Histogram of Muscle Mass (kg)")

ggplot(data, aes(x = bmi)) + 
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") + 
  labs(title = "Histogram of BMI")


# Boxplots

ggplot(long_data, aes(x = Nutrient, y = Intake, fill = Nutrient)) +
  geom_boxplot() +
  labs(title = "Boxplot of Macronutrient Intake",
       x = "Nutrient",
       y = "Intake (g)") +
  scale_fill_manual(values = c(
    "protein_intake_g" = "orange",
    "fat_intake_g" = "plum",
    "carbohydrate_intake_g" = "steelblue"
  )) +
  theme_minimal()


# Scatter Plot: Caloric Intake vs BMI

ggplot(data, aes(x = daily_caloric_intake_kcal, y = bmi)) +
  geom_point(color = "darkblue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Caloric Intake vs BMI",
       x = "Daily Caloric Intake (kcal)",
       y = "BMI")


# Scatter Plot: Muscle Mass vs Protein Intake

ggplot(data, aes(x = muscle_mass_kg, y = protein_intake_g)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Muscle Mass vs Protein Intake",
       x = "Muscle Mass (kg)",
       y = "Protein Intake (g)")


# Correlation Test: Caloric Intake vs BMI

cor_test_result <- cor.test(data$daily_caloric_intake_kcal, data$bmi)
print(cor_test_result)

# Interpretation
if (cor_test_result$p.value < 0.05) {
  cat("\nSignificant correlation found between daily caloric intake and BMI.\n")
} else {
  cat("\nNo significant correlation found between daily caloric intake and BMI.\n")
}

