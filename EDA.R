# Load packages
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(grid)

# Load data
path<- 'C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/'
file<-'file_list.csv'
move<-'movement_preprocess.csv'
quest<-'questionnaire_preprocess.csv'
data<-read.csv(paste0(path, file), header = T)
movement<-read.csv(paste0(path, move), header = T)
quest<-read.csv(paste0(path, quest))

dim(data)

unique(data$resource_type)
unique(data$id)
unique(data$study_id)
unique(data$condition)
unique(data$effect_of_alcohol_on_tremor)

table(data$label)

#####----Simple Manipulation-----
# Calculate variables
# age of PD duration - healthy is 0
data['duration']<-data['age']-data['age_at_diagnosis']

# there is a typo of height at obs 227: 55 -> 155
row_with_typo <- which(data$height == 55)
data$height[row_with_typo] <- 155

# Convert `label` into a factor with meaningful labels
data$diag <- factor(data$label, levels = c(0, 1, 2), labels = c("HC", "PD", "DD"))

# Add questionnaire total scores
data$quest_score<-quest$total_score

# Add label to move and quest data
movement$diag<-data$diag
quest$diag<-data$diag

###----Patient Data----
# 1. Barplot for `condition`
p1<-ggplot(data, aes(x = factor(condition))) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Distribution of Condition", x = "Condition", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
p1
ggsave(paste0(path, 'condition.png'), plot = p1, width = 12, height = 9, dpi = 300)

# 2. Barplot for `label`
p2<-ggplot(data, aes(x = factor(label, labels = c("HC", "PD", "DD")))) +
  geom_bar(fill = "coral", color = "black") +
  labs(title = "Distribution of Labels", x = "Diagnosis", y = "Count") +
  theme_minimal()
p2
ggsave(paste0(path, 'labels.png'), plot = p2, width = 12, height = 9, dpi = 300)


# 3. Barplot for `gender`
p3<-ggplot(data, aes(x = factor(gender))) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count") +
  theme_minimal()
p3
ggsave(paste0(path, 'gender.png'), plot = p3, width = 12, height = 9, dpi = 300)


# 4. Barplot for `handedness`
p4<-ggplot(data, aes(x = factor(handedness))) +
  geom_bar(fill = "orange", color = "black") +
  labs(title = "Distribution of Handedness", x = "Handedness", y = "Count") +
  theme_minimal()
p4
ggsave(paste0(path, 'handedness.png'), plot = p4, width = 12, height = 9, dpi = 300)


# 5. Histogram for `age_at_diagnosis`
p5<-ggplot(data, aes(x = age_at_diagnosis)) +
  geom_histogram(fill = "skyblue", color = "black", bins = 30) +
  labs(title = "Age of Diagnosis", x = "Year", y = "Frequency") +
  theme_minimal()
p5
ggsave(paste0(path, 'age_at_diagnosis.png'), plot = p5, width = 12, height = 9, dpi = 300)


# 6. Histogram for `height`
p6<-ggplot(data, aes(x = height)) +
  geom_histogram(fill = "purple", color = "black", bins = 30) +
  labs(title = "Height", x = "cm", y = "Frequency") +
  theme_minimal()
p6
ggsave(paste0(path, 'height.png'), plot = p6, width = 12, height = 9, dpi = 300)


# 7. Histogram for `weight`
p7<-ggplot(data, aes(x = weight)) +
  geom_histogram(fill = "brown", color = "black", bins = 30) +
  labs(title = "Weight", x = "kg", y = "Frequency") +
  theme_minimal()
p7
ggsave(paste0(path, 'weight.png'), plot = p7, width = 12, height = 9, dpi = 300)

# 8. Histogram for `duration`
p8<-ggplot(data, aes(x = duration)) +
  geom_histogram(fill = "pink", color = "black", bins = 30) +
  labs(title = "Duration", x = "Year", y = "Frequency") +
  theme_minimal()
p8
ggsave(paste0(path, 'duration.png'), plot = p8, width = 12, height = 9, dpi = 300)


# 9. 'appearance_in_kinship' and disease diagnosis
p9<-ggplot(data, aes(x = appearance_in_kinship, fill = diag)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Barplot: Appearance in Kinship",
    x = "Appearance in Kinship (TRUE/FALSE)",
    y = "Count",
    fill = "Diagnosis"
  ) +
  scale_fill_manual(values = c("HC" = "blue4", "PD" = "red4", "DD" = "green4")) +
  theme_minimal()
p9
ggsave(paste0(path, 'kinship.png'), plot = p9, width = 12, height = 9, dpi = 300)


# 10. 'effect_of_alcohol_on_tremor' and disease diagnosis
p10<-ggplot(data, aes(x = effect_of_alcohol_on_tremor, fill = diag)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Barplot: Effect of Alcohol on Tremor",
    x = "Effect of Alcohol on Tremor",
    y = "Count",
    fill = "Diagnosis"
  ) +
  scale_fill_manual(values = c("HC" = "blue4", "PD" = "red4", "DD" = "green4")) +
  theme_minimal()
p10
ggsave(paste0(path, 'alcohol.png'), plot = p10, width = 12, height = 9, dpi = 300)


# 11. boxplot of diagnosis and duration
p11<-ggplot(data, aes(x = factor(label, labels = c("HC", "PD", "DD")), y = duration)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Duration by Diagnosis",
    x = "Diagnosis (Label)",
    y = "Year"
  ) +
  theme_minimal()
ggsave(paste0(path, 'diag_and_duration.png'), plot = p11, width = 12, height = 9, dpi = 300)


# 12. boxplot of diagnosis and height
p12<-ggplot(data, aes(x = factor(label, labels = c("HC", "PD", "DD")), y = height)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Height by Diagnosis",
    x = "Diagnosis (Label)",
    y = "cm"
  ) +
  theme_minimal()
p12
ggsave(paste0(path, 'diag_and_height.png'), plot = p12, width = 12, height = 9, dpi = 300)


# 13. boxplot of diagnosis and weight
p13<-ggplot(data, aes(x = factor(label, labels = c("HC", "PD", "DD")), y = weight)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of weight by Diagnosis",
    x = "Diagnosis (Label)",
    y = "kg"
  ) +
  theme_minimal()
p13
ggsave(paste0(path, 'diag_and_weight.png'), plot = p13, width = 12, height = 9, dpi = 300)


# 14. 'handedness' and disease diagnosis
p14<-ggplot(data, aes(x = handedness, fill = diag)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Barplot: Handedness",
    x = "Handedness (Left/Right)",
    y = "Count",
    fill = "Diagnosis"
  ) +
  scale_fill_manual(values = c("HC" = "blue4", "PD" = "red4", "DD" = "green4")) +
  theme_minimal()
p14
ggsave(paste0(path, 'diag_and_hand.png'), plot = p14, width = 12, height = 9, dpi = 300)

# 15. 'gender' and disease diagnosis
p15<-ggplot(data, aes(x = gender, fill = diag)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Barplot: Gender",
    x = "Gender (Male/Female)",
    y = "Count",
    fill = "Diagnosis"
  ) +
  scale_fill_manual(values = c("HC" = "blue4", "PD" = "red4", "DD" = "green4")) +
  theme_minimal()
p15
ggsave(paste0(path, 'diag_and_gender.png'), plot = p15, width = 12, height = 9, dpi = 300)


####-----Questionnaire data----
# 1. Boxplot of diagnosis and questionnaire score
q1<-ggplot(data, aes(x = factor(label, labels = c("HC", "PD", "DD")), y = quest_score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Distribution of Questionnaire Scores by Label",
    x = "Diagnosis (Label)",
    y = "Total Questionnaire Score"
  ) +
  theme_minimal()
q1
ggsave(paste0(path, 'diag_quest_score.png'), plot = q1, width = 12, height = 9, dpi = 300)


# 2. Distribution and boxplot of categories
# List of categories to plot
categories <- c(
  "Gastrointestinal_tract", "Urinal_tract", "Pain", "Miscellaneous",
  "Apathy_Attention_Memory", "Distortion_of_perception",
  "Depression_Anxiety", "Sexual_Function", "Cardiovascular", "Sleep_Fatigue"
)

# Loop through each category and create the plots
# Loop through each category and save the plots
for (cat in categories) {
  # Use tidy evaluation to map the column names dynamically
  cat_sym <- sym(cat)
  
  # Left plot: Distribution of scores
  p1 <- ggplot(quest, aes(x = !!cat_sym)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(
      title = paste("Distribution of", cat),
      x = "Scores",
      y = "Frequency"
    ) +
    theme_minimal()
  
  # Right plot: Boxplot of scores by diagnosis
  p2 <- ggplot(quest, aes(x = diag, y = !!cat_sym)) +
    geom_boxplot(fill = "lightgreen", color = "black") +
    labs(
      title = paste("Scores by Diagnosis for", cat),
      x = "Diagnosis",
      y = "Scores"
    ) +
    theme_minimal()
  
  # Combine the two plots side by side
  combined_plot <- grid.arrange(p1, p2, ncol = 2)
  
  # Save the combined plot as a PNG file
  output_file <- paste0("./plots/", cat, ".png")
  ggsave(output_file, plot = combined_plot, width = 12, height = 6, dpi = 300)
}

####-----Time Series Example: PD and HC----
# Example: obs1 - healthy / obs6 - PD
time<-'C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/archive/movement/timeseries/'
obs1<-'001_Relaxed_RightWrist.txt'
obs6<-'006_Relaxed_RightWrist.txt'

healthy<-read.table(paste0(time, obs1), sep = ',', header = F,
                        col.names = c('time', 'acc_x', 'acc_y', 'acc_z', 'gy_x', 'gy_y', 'gy_z'))
parkinson<-read.table(paste0(time, obs6), sep = ',', header = F,
                    col.names = c('time', 'acc_x', 'acc_y', 'acc_z', 'gy_x', 'gy_y', 'gy_z'))


# par(mfrow = c(1, 2))
# Healthy patient is normal when relaxed
# Accelerometer values (acc_x, acc_y, acc_z)
plot(healthy$time, healthy$acc_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values", ylim = c(-0.2, 0.2),
     main = "Accelerometer Values Over Time (Healthy)")
lines(healthy$time, healthy$acc_y, col = "blue4")
lines(healthy$time, healthy$acc_z, col = "green4")
legend("topright", legend = c("acc_x", "acc_y", "acc_z"), col = c("red4", "blue4", "green4"), lty = 1, cex = 0.6)

# Gyroscope values (gy_x, gy_y, gy_z)
plot(healthy$time, healthy$gy_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values",  ylim = c(-0.2, 0.2),
     main = "Gyroscope Values Over Time (Healthy)")
lines(healthy$time, healthy$gy_y, col = "blue4")
lines(healthy$time, healthy$gy_z, col = "green4")
legend("topright", legend = c("gy_x", "gy_y", "gy_z"), col = c("red4", "blue4", "green4"), lty = 1, cex = 0.6)

# Parkinson's patient has tremor when relaxed
# Plot 1: Accelerometer values (acc_x, acc_y, acc_z)
plot(parkinson$time, parkinson$acc_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values", ylim = c(-0.2, 0.2),
     main = "Accelerometer Values Over Time (PD)")
lines(parkinson$time, parkinson$acc_y, col = "blue4")
lines(parkinson$time, parkinson$acc_z, col = "green4")
legend("topright", legend = c("acc_x", "acc_y", "acc_z"), col = c("red4", "blue4", "green4"), lty = 1, cex = 0.6)

# Plot 2: Gyroscope values (gy_x, gy_y, gy_z)
plot(parkinson$time, parkinson$gy_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values",  ylim = c(-0.2, 0.2),
     main = "Gyroscope Values Over Time (PD)")
lines(parkinson$time, parkinson$gy_y, col = "blue4")
lines(parkinson$time, parkinson$gy_z, col = "green4")
legend("topright", legend = c("gy_x", "gy_y", "gy_z"), col = c("red4", "blue4", "green4"), lty = 1, cex = 0.6)

# par(mfrow = c(1, 1))

####----Time Series----
task <- c('CrossArms', 'DrinkGlas', 'Entrainment', 'HoldWeight', 'LiftHold',
          'PointFinger', 'Relaxed', 'RelaxedTask', 'StretchHold', 'TouchIndex',
          'TouchNose')
hand <- c('LeftWrist', 'RightWrist')
summary_statistics <- c('sum_abs_energy', 'stdev', 'max_abs_amp', 'sum_psd')
channels <- c('accelerometer', 'gyroscope')

# Loop through tasks and summary statistics
for (stat in summary_statistics) {
  for (t in task) {
    # Generate column names for all four combinations
    cols <- list(
      LeftWrist_accelerometer = paste(t, "LeftWrist", stat, "accelerometer", sep = "_"),
      RightWrist_accelerometer = paste(t, "RightWrist", stat, "accelerometer", sep = "_"),
      LeftWrist_gyroscope = paste(t, "LeftWrist", stat, "gyroscope", sep = "_"),
      RightWrist_gyroscope = paste(t, "RightWrist", stat, "gyroscope", sep = "_")
    )
    
    # Create plots for each of the four configurations
    p1 <- ggplot(movement, aes(x = diag, y = .data[[cols$LeftWrist_accelerometer]])) +
      geom_boxplot(fill = "lightblue", color = "black") +
      labs(
        title = paste("Left Wrist - Accelerometer"),
        x = "Diagnosis",
        y = stat
      ) +
      theme_minimal()
    
    p2 <- ggplot(movement, aes(x = diag, y = .data[[cols$RightWrist_accelerometer]])) +
      geom_boxplot(fill = "lightgreen", color = "black") +
      labs(
        title = paste("Right Wrist - Accelerometer"),
        x = "Diagnosis",
        y = stat
      ) +
      theme_minimal()
    
    p3 <- ggplot(movement, aes(x = diag, y = .data[[cols$LeftWrist_gyroscope]])) +
      geom_boxplot(fill = "pink", color = "black") +
      labs(
        title = paste("Left Wrist - Gyroscope"),
        x = "Diagnosis",
        y = stat
      ) +
      theme_minimal()
    
    p4 <- ggplot(movement, aes(x = diag, y = .data[[cols$RightWrist_gyroscope]])) +
      geom_boxplot(fill = "lightyellow", color = "black") +
      labs(
        title = paste("Right Wrist - Gyroscope"),
        x = "Diagnosis",
        y = stat
      ) +
      theme_minimal()
    
    # Combine the plots in a 2x2 grid
    combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
    
    # Add a bottom-centered title for the task
    task_title <- textGrob(
      label = paste("Task:", t),
      gp = gpar(fontsize = 14, fontface = "bold"),
      vjust = 1
    )
    combined_with_title <- grid.arrange(combined_plot, task_title, ncol = 1, heights = c(10, 1))
    
    # Save the plot with the task title as PNG
    output_file <- paste0("./plots/", t, "_", stat, ".png")
    ggsave(output_file, plot = combined_with_title, width = 12, height = 9, dpi = 300)
  }
}
