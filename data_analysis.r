####----Load Libraries----
library(agricolae)  # For LSD test
library(VGAM) # for multinomial logit
library(MASS)
library(corrplot)
library(nnet)
library(caret) # for confusion matrix

####----Load Data----
path<- 'C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/'
file<-'file_list.csv'
move<-'movement_preprocess.csv'
quest<-'questionnaire_preprocess.csv'
data<-read.csv(paste0(path, file), header = T)
movement<-read.csv(paste0(path, move), header = T)
quest<-read.csv(paste0(path, quest))

####----various lists----
categories <- c(
  "Gastrointestinal_tract", "Urinal_tract", "Pain", "Miscellaneous",
  "Apathy_Attention_Memory", "Distortion_of_perception",
  "Depression_Anxiety", "Sexual_Function", "Cardiovascular", "Sleep_Fatigue"
)


####----Simple Preprocessing----
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

####----1. chi-square test on questionnaire----
# Initialize an empty dataframe for the results
output <- data.frame(
  HC_TRUE = integer(),
  HC_FALSE = integer(),
  PD_TRUE = integer(),
  PD_FALSE = integer(),
  DD_TRUE = integer(),
  DD_FALSE = integer(),
  chisq_pvalue = numeric()
)

# Iterate through each question column
for (question in paste0("Q", 1:30)) {
  print(question)
  # Create a contingency table
  contingency_table <- table(quest[[question]], quest$diag)
  
  # Ensure the contingency table includes all levels for diag and answers (0, 1)
  contingency_table <- as.data.frame.matrix(contingency_table)
  contingency_table[["HC"]] <- contingency_table[["HC"]] %||% 0
  contingency_table[["PD"]] <- contingency_table[["PD"]] %||% 0
  contingency_table[["DD"]] <- contingency_table[["DD"]] %||% 0
  contingency_table <- contingency_table[match(c(0, 1), rownames(contingency_table)), ]
  
  # Extract counts for HC, PD, DD (TRUE/FALSE)
  hc_true <- contingency_table["1", "HC"]
  hc_false <- contingency_table["0", "HC"]
  pd_true <- contingency_table["1", "PD"]
  pd_false <- contingency_table["0", "PD"]
  dd_true <- contingency_table["1", "DD"]
  dd_false <- contingency_table["0", "DD"]
  
  # Perform chi-square test
  chisq_test <- chisq.test(contingency_table, correct = T)
  chisq_pvalue <- chisq_test$p.value
  
  # Add results to the output dataframe
  output <- rbind(output, data.frame(
    HC_TRUE = hc_true,
    HC_FALSE = hc_false,
    PD_TRUE = pd_true,
    PD_FALSE = pd_false,
    DD_TRUE = dd_true,
    DD_FALSE = dd_false,
    chisq_pvalue = chisq_pvalue
  ))
}

# Add row names as Q1-Q30
rownames(output) <- paste0("Q", 1:30)

# View the output
print(output)

# save output
write.csv(output, file = 'questionnaire_chisq_results.csv', row.names = T)

####----2. ANOVA----
###----2.1 questionnaire----
# Add total_score column to the list of categories
variables <- c(categories, "total_score")

# Initialize an empty dataframe for results
anova_results <- data.frame(
  F_stat = numeric(11),
  p_value = numeric(11),
  groupings = character(11),
  row.names = variables
)

# Loop through each category
for (cat in variables) {
  # print(cat)
  
  # Perform ANOVA for the category
  anova_model <- aov(quest[[cat]] ~ quest$diag)
  anova_summary <- summary(anova_model)
  
  # Extract F-statistic and p-value
  F_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  # Perform LSD test
  lsd_test <- LSD.test(anova_model, "quest$diag", console = FALSE)
  # print(class(lsd_test$groups$groups))
  
  # Extract groupings from the LSD test
  group_labels <- rownames(lsd_test$groups)
  group_values <- lsd_test$groups$groups
  grouping_string <- paste(paste(group_labels, collapse = " "), "/", 
                           paste(group_values, collapse = " "))
  
  # Add results to the dataframe
  anova_results[cat, ] <- c(F_stat, p_value, grouping_string)
}

# Rename columns for clarity
colnames(anova_results) <- c("F_stat", "p_value", "groupings")

# View the results
print(anova_results)

# save results
write.csv(anova_results, file = 'questionnaire_anova_results.csv', row.names = T)


###----2.2 movement: separate hands----
# Extract variable names (excluding the 'diag' column)
variables <- colnames(movement)[colnames(movement) != "diag"]

# Initialize an empty dataframe for results
anova_results <- data.frame(
  F_stat = numeric(length(variables)),
  p_value = numeric(length(variables)),
  groupings = character(length(variables)),
  row.names = variables
)

# Loop through each variable
for (cat in variables) {
  # Print variable name for debugging
  # print(cat)
  
  # Perform ANOVA for the variable
  anova_model <- aov(movement[[cat]] ~ movement$diag)
  anova_summary <- summary(anova_model)
  
  # Extract F-statistic and p-value
  F_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  # Perform LSD test
  lsd_test <- LSD.test(anova_model, "movement$diag", console = FALSE)
  print(lsd_test$groups)
  
  # Construct the grouping string
  group_labels <- rownames(lsd_test$groups)
  print(group_labels)
  group_values <- lsd_test$groups$groups
  grouping_string <- paste(paste(group_labels, collapse = " "), "/", paste(group_values, collapse = " "))
  
  # Add results to the dataframe
  anova_results[cat, ] <- c(F_stat, p_value, grouping_string)
}

# Rename columns for clarity
colnames(anova_results) <- c("F_stat", "p_value", "groupings")

# View the results
print(anova_results)

# save results
write.csv(anova_results, file = 'movement_separate_anova_results.csv', row.names = T)


####----2.3 movement: both hands----
# Initialize a new dataframe for storing averaged results
movement2 <- data.frame(diag = movement$diag)  # Start with the diag column

# Extract the unique components of the variable names
tasks <- unique(gsub("_.*", "", colnames(movement)[colnames(movement) != "diag"]))
summary_statistics <- c("sum_abs_energy", "stdev", "max_abs_amp", "sum_psd")
channels <- c("accelerometer", "gyroscope")

# Loop over tasks, summary statistics, and channels
for (task in tasks) {
  for (stat in summary_statistics) {
    for (channel in channels) {
      # Construct variable names for LeftWrist and RightWrist
      left_var <- paste(task, "LeftWrist", stat, channel, sep = "_")
      right_var <- paste(task, "RightWrist", stat, channel, sep = "_")
      
      # Check if both variables exist in the dataset
      if (left_var %in% colnames(movement) && right_var %in% colnames(movement)) {
        # Compute the mean of LeftWrist and RightWrist
        mean_var <- paste(task, stat, channel, sep = "_")
        movement2[[mean_var]] <- rowMeans(movement[, c(left_var, right_var)], na.rm = TRUE)
      }
    }
  }
}
# save dataframe just in case
write.csv(movement2, "movement_preprocessed_averaged.csv", row.names = FALSE)

# Extract variable names for ANOVA (excluding the diag column)
variables <- colnames(movement2)[colnames(movement2) != "diag"]

# Initialize an empty dataframe for ANOVA results
anova_results <- data.frame(
  F_stat = numeric(length(variables)),
  p_value = numeric(length(variables)),
  groupings = character(length(variables)),
  row.names = variables
)

# Loop through each variable for ANOVA
for (cat in variables) {
  # Perform ANOVA
  anova_model <- aov(movement2[[cat]] ~ movement2$diag)
  anova_summary <- summary(anova_model)
  
  # Extract F-statistic and p-value
  F_stat <- anova_summary[[1]]$`F value`[1]
  p_value <- anova_summary[[1]]$`Pr(>F)`[1]
  
  # Perform LSD test for multiple comparisons
  lsd_test <- LSD.test(anova_model, "movement2$diag", console = FALSE)
  
  # Construct the grouping string
  group_labels <- rownames(lsd_test$groups)
  group_values <- lsd_test$groups$groups
  grouping_string <- paste(paste(group_labels, collapse = " "), "/", paste(group_values, collapse = " "))
  
  # Add results to the dataframe
  anova_results[cat, ] <- c(F_stat, p_value, grouping_string)
}

# Rename columns for clarity
colnames(anova_results) <- c("F_stat", "p_value", "groupings")

# View results
print(anova_results)

# save results
write.csv(anova_results, file = 'movement_averaged_anova_results.csv', row.names = T)


####----3. multinomial logit----
# select significant variables
r1<-'questionnaire_anova_results.csv'
r2<-'movement_averaged_anova_results.csv'

questionnaire_anova_results<-read.csv(paste0(path, r1), header = T)
movement_averaged_anova_results<-read.csv(paste0(path, r2), header = T)

# all categories are significant on ANOVA on questionnaire
# pick the one that defines three groups in LSD
quest_significant<-c('Gastrointestinal_tract', 'Urinal_tract', 'Miscellaneous',
                     'Distortion_of_perception', 'Sleep_Fatigue', 'total_score')

move_significant<-as.vector(movement_averaged_anova_results[movement_averaged_anova_results$p_value < 0.05, ]$X)

# if you fit the vglm, it will not converge because of multicollinearity
# thus choose one statistic from each task. sum_abs_energy was chosen all significant variables had at least 
# sub_abs_energy
move_chosen<-c('CrossArms_sum_abs_energy_gyroscope','DrinkGlas_sum_abs_energy_gyroscope',
               'Entrainment_sum_abs_energy_gyroscope', 'HoldWeight_sum_abs_energy_gyroscope',
               'LiftHold_sum_abs_energy_gyroscope', 'PointFinger_sum_abs_energy_gyroscope',
              'Relaxed_sum_abs_energy_gyroscope',  'RelaxedTask_sum_abs_energy_gyroscope',
               'StretchHold_sum_abs_energy_gyroscope', 'TouchNose_sum_abs_energy_gyroscope')

data_significant<-c('height', 'weight', 'gender', 
                    'handedness', 'appearance_in_kinship',
                    'duration', 'diag') # drop age and age of diagnosis

# create final dataset
final<-cbind(data[,data_significant], movement2[,move_significant], quest[,quest_significant])
write.csv(final, file = 'final_data.csv')

final2<-cbind(data[,data_significant], movement2[,move_chosen], quest[,quest_significant])
write.csv(final, file = 'final_data2.csv')

# set baseline to HC
final2$diag <- relevel(factor(final$diag), ref = "HC")

# plot correlation plot
# Compute the correlation matrix
numeric_data <- final2[, sapply(final2, is.numeric)]  # Select only numeric columns
cor_matrix <- cor(numeric_data, use = "complete.obs")         # Calculate correlation matrix
View(cor_matrix)

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, # Text color and rotation
         addCoef.col = "black")        # Add correlation coefficients


# VGAM original trial
####----3.1 initial model----
# Fit the initial multinomial logistic regression model from all variables
initial_model <- vglm(
  diag ~ .,  # Include all predictors
  family = multinomial(refLevel = 1),  # Specify the baseline
  data = final
)
summary(initial_model)

####----3.2 second model----
# choose variables based on reasoning
second_model<-vglm(
  diag~.,
  family = multinomial(refLevel = 1),
  data = final2
)
summary(second_model)

####----3.3 optimal model----
# choose the significant variables from second_model
optimal_model <- vglm(
  diag ~ duration + total_score + LiftHold_sum_abs_energy_gyroscope,
  family = multinomial(refLevel = 1),
  data = final2
)

# summary of the optimal model
summary(optimal_model)

####----3.4 model evaluation
# Function to calculate predictions and confusion matrix
evaluate_model <- function(model, data, true_labels) {
  # Predict the classes
  predictions <- predict(model, newdata = data, type = "response")
  
  # Convert probabilities to class predictions (highest probability per row)
  predicted_classes <- apply(predictions, 1, function(x) colnames(predictions)[which.max(x)])
  
  # Create a confusion matrix
  cm <- confusionMatrix(factor(predicted_classes, levels = levels(true_labels)),
                        factor(true_labels, levels = levels(true_labels)))
  
  # Return confusion matrix
  return(cm)
}

# Model 1: Initial model on `final`
model1_cm <- evaluate_model(initial_model, final, final$diag)

# Model 2: Refined model on `final2`
model2_cm <- evaluate_model(second_model, final2, final2$diag)

# Model 3: Optimal model on `final2`
opt_vars<-c('diag', 'duration', 'total_score', 'LiftHold_sum_abs_energy_gyroscope')
final3<-final2[,opt_vars]
model3_cm <- evaluate_model(optimal_model, final3, final3$diag)

# Display confusion matrices
print("Confusion Matrix for Model 1")
print(model1_cm)

print("Confusion Matrix for Model 2")
print(model2_cm)

print("Confusion Matrix for Model 3")
print(model3_cm)

# Optional: Extract overall accuracy
model1_accuracy <- model1_cm$overall['Accuracy']
model2_accuracy <- model2_cm$overall['Accuracy']
model3_accuracy <- model3_cm$overall['Accuracy']

print("Overall Accuracy:")
print(c(Model1 = model1_accuracy, Model2 = model2_accuracy, Model3 = model3_accuracy))

# Function to plot a confusion matrix
plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  # Extract the confusion matrix table as a dataframe
  cm_table <- as.data.frame(cm$table)
  colnames(cm_table) <- c("Reference", "Prediction", "Freq")
  
  # Plot confusion matrix using ggplot
  ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() +
    labs(title = title, x = "True Class", y = "Predicted Class") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Plot confusion matrices for each model
print("Confusion Matrix for Model 1")
pm1<-plot_confusion_matrix(model1_cm, title = "Confusion Matrix for Model 1 (Initial Model)")
ggsave('confusion_matrix_initial.png', plot = pm1, width = 4, height = 3, dpi = 300)

print("Confusion Matrix for Model 2")
pm2<-plot_confusion_matrix(model2_cm, title = "Confusion Matrix for Model 2 (Refined Model)")
ggsave('confusion_matrix_second.png', plot = pm2, width = 4, height = 3, dpi = 300)


print("Confusion Matrix for Model 3")
pm3<-plot_confusion_matrix(model3_cm, title = "Confusion Matrix for Model 3 (Optimal Model)")
ggsave('confusion_matrix_third.png', plot = pm3, width = 4, height = 3, dpi = 300)


# nnet trial
# # Function to extract Wald test results from multinom model
# wald_test_results <- function(model) {
#   # Extract coefficients and standard errors
#   coef_matrix <- summary(model)$coefficients
#   se_matrix <- summary(model)$standard.errors
#   
#   # Calculate Wald z-statistics and p-values
#   wald_z <- coef_matrix / se_matrix
#   wald_p <- 2 * pnorm(-abs(wald_z))
#   
#   # Combine results into a list
#   results <- list(
#     coefficients = coef_matrix,
#     standard_errors = se_matrix,
#     wald_z = wald_z,
#     wald_p = wald_p
#   )
#   
#   # Return the results
#   return(results)
# }
# 
# # Fit the initial multinomial logistic regression model using nnet
# initial_model <- multinom(diag ~ ., data = final, maxit = 1000)
# 
# # Summary of the initial model
# summary(initial_model)
# wald_test_results(initial_model)
# 
# # Fit a refined model
# refined_model <- multinom(diag ~ ., data = final2)
# summary(refined_model)
# wald_test_results(refined_model)
# 
# # Stepwise variable selection using stepAIC
# optimal_model <- stepAIC(
#   refined_model,
#   direction = "both",  # Both forward and backward selection
#   trace = TRUE         # Display progress
# )
# 
# # Summary of the optimal model
# summary(optimal_model)
# wald_test_results(optimal_model)