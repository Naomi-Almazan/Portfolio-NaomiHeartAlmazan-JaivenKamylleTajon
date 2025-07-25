library(dplyr)
library(tidyr)
library(ggplot2)
df <- read.csv("C:/Users/USER/Downloads/BI120L_Datasets/Dataset_even.csv", stringsAsFactors = TRUE)

#max scaling stress and heart rate
df$Scaled_Stress <- df$Stress_Level / max(df$Stress_Level, na.rm = TRUE)
df$Scaled_HeartRate <- df$Heart_rate / max(df$Heart_rate, na.rm = TRUE)

df$Sleep_Category <- cut(df$Daily_Sleeping_hours,
                         breaks = c(-Inf, 6, 9, Inf),
                         labels = c("Insufficient", "Normal", "Excess"))
avg_values <- df %>%
  group_by(Sleep_Category) %>%
  summarise(
    Avg_Scaled_Stress = mean(Scaled_Stress, na.rm = TRUE),
    Avg_Scaled_HeartRate = mean(Scaled_HeartRate, na.rm = TRUE)
  )

df$Sleep_Category <- cut(df$Daily_Sleeping_hours,
                         breaks = c(-Inf, 6, 9, Inf),
                         labels = c("Insufficient", "Normal", "Excess"))
avg_values <- df %>%
  group_by(Sleep_Category) %>%
  summarise(
    Avg_Scaled_Stress = mean(Scaled_Stress, na.rm = TRUE),
    Avg_Scaled_HeartRate = mean(Scaled_HeartRate, na.rm = TRUE)
  )
avg_long <- avg_values %>%
  pivot_longer(cols = -Sleep_Category, names_to = "Metric", values_to = "Value")

# Plot with value labels
ggplot(avg_long, aes(x = Sleep_Category, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(Value, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3) +
  labs(title = "Average Scaled Stress and Heart Rate by Sleep Category",
       x = "Sleep Category", y = "Scaled Value (0 to 1)") +
  theme_minimal() +
  scale_fill_manual(values = c("Avg_Scaled_Stress" = "steelblue",
                               "Avg_Scaled_HeartRate" = "tomato")) +
  theme(legend.title = element_blank())

insufficient <- df[df$Sleep_Category == "Insufficient", ]
normal <- df[df$Sleep_Category == "Normal", ]

# Subset data by sleep category
insufficient <- df[df$Sleep_Category == "Insufficient", ]
normal <- df[df$Sleep_Category == "Normal", ]

# T-test for Heart Rate: Insufficient vs Normal sleep
t_test_heart <- t.test(insufficient$Heart_rate, normal$Heart_rate)
print(t_test_heart)

# T-test for Stress Level: Insufficient vs Normal sleep
t_test_stress <- t.test(insufficient$Stress_Level, normal$Stress_Level)
print(t_test_stress)