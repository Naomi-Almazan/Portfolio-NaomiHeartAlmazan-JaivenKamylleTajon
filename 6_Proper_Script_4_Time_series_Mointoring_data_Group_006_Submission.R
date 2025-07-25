library(dplyr)
library(tidyr)
library(ggplot2)
df <- read.csv("C:/Users/USER/Downloads/BI120L_Datasets/4_Time_series_Mointoring_data_Group_006.csv", stringsAsFactors = TRUE)

#Select data
df_long <- df %>%
  select(Month_numerical,
         Patient_1_avg_steps, Patient_2_avg_steps, Patient_3_avg_steps,
         Patient_1_Stress_Level, Patient_2_Stress_Level, Patient_3_Stress_Level,
         Patient_1_BMI, Patient_2_BMI, Patient_3_BMI) %>%
  pivot_longer(
    cols = -Month_numerical,
    names_to = c("Patient", "Variable"),
    names_pattern = "Patient_(\\d+)_(.+)",
    values_to = "Value"
  ) %>%
  mutate(Patient = paste0("Patient_", Patient)) %>%
  pivot_wider(names_from = Variable, values_from = Value)

# Bubble plot (all patients, all months, same scale)
ggplot(df_long, aes(
  x = avg_steps,
  y = Stress_Level,
  size = BMI,
  fill = Patient
)) +
  geom_point(alpha = 0.6, shape = 21, color = "black") +
  scale_size_continuous(range = c(3, 15)) +
  labs(
    title = "Step Count vs Stress Level (Bubble = BMI)",
    x = "Step Count",
    y = "Stress Level",
    size = "BMI"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.title = element_blank())
# Create a list of patients
patients <- c("Patient_1", "Patient_2", "Patient_3")

# Loop through each patient and perform correlation tests
for (p in patients) {
  # Extract relevant columns
  steps <- df[[paste0(p, "_avg_steps")]]
  stress <- df[[paste0(p, "_Stress_Level")]]
  bmi <- df[[paste0(p, "_BMI")]]
  
  # Print patient name
  cat("\n===== Correlation Tests for", p, "=====\n")
  
  # Steps vs Stress
  cor1 <- cor.test(steps, stress, method = "pearson")
  cat("Steps vs Stress:\n")
  print(cor1)
  
  # Steps vs BMI
  cor2 <- cor.test(steps, bmi, method = "pearson")
  cat("Steps vs BMI:\n")
  print(cor2)
  
  # Stress vs BMI
  cor3 <- cor.test(stress, bmi, method = "pearson")
  cat("Stress vs BMI:\n")
  print(cor3)
}