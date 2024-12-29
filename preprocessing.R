library(dplyr)
library(jsonlite)
library(ggplot2)
library(gsignal) # For Welch's method

####---File Paths----
movement<-"C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/archive/movement/timeseries/"
questionnaire<-"C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/archive/questionnaire/"

####----Data preprocess scheme----
nPat<-1:469

####----Create empty dataset----
# 22 datasets per obs * 4 summary statistics * 2 channels
move_all_data<-matrix(NA, nrow = 469, ncol = 22*4*2) 
quest_all_data <- matrix(NA, nrow = 469, ncol = 31)

####----All task and hand name combinations - for file calling----
task<-c('CrossArms', 'DrinkGlas', 'Entrainment', 'HoldWeight', 'LiftHold',
        'PointFinger', 'Relaxed', 'RelaxedTask', 'StretchHold', 'TouchIndex',
        'TouchNose')
hand<-c('LeftWrist', 'RightWrist')
summary_statistics<-c('sum_abs_energy', 'stdev', 'max_abs_amp', 'sum_psd')
channels<-c('accelerometer', 'gyroscope')
timeseries <- c()
all_summary_statistics <- c()
all_columns <- c()

# Use a nested for loop to generate combinations of dataset names
for (t in task) {
  for (h in hand) {
    timeseries <- c(timeseries, paste(t, h, sep = "_"))
  }
}

# Use a nested for loop to generate combinations of summary statistics of movement
for (t in timeseries){
  for (s in summary_statistics){
    all_summary_statistics <- c(all_summary_statistics, paste(t, s, sep = "_"))
  }
}

# Use a nested for loop to generate combinations of all columns
for (a in all_summary_statistics){
  for (c in channels){
    all_columns <- c(all_columns, paste(a, c, sep = "_"))
  }
}

####----Preprocessing all patients----
for(i in nPat){
  # Format file name
  formatted_number <- sprintf("%03d", i)
  print(formatted_number)
  obs_m<-paste0('observation_', formatted_number, '.json')
  obs_q<-paste0('questionnaire_response_', formatted_number, '.json')
  
  # Movement Data
  obs_move_summary<-c()
  for (name in timeseries){
    timeseries_data<-read.table(paste0(movement, formatted_number, '_', name, '.txt'),
                                col.names = c('time', 'acc_x', 'acc_y', 'acc_z', 'gy_x', 'gy_y', 'gy_z'),
                                sep = ',', header = F)
    abs_energy<-apply(abs(timeseries_data[,-1]), MARGIN = 2, FUN = sum) # columnwise sum of absolute values
    abs_energy<-c(sum(abs_energy[1:3]), sum(abs_energy[4:6])) # add x, y, z axes (acc , gy)
    
    std_dev <- apply(timeseries_data[,-1], MARGIN = 2, FUN = sd)
    std_dev<-c(sum(std_dev[1:3]), sum(std_dev[4:6]))
    
    max_abs_amp<-apply(abs(timeseries_data[,-1]), MARGIN = 2, FUN = max)
    max_abs_amp<-c(sum(max_abs_amp[1:3]), sum(max_abs_amp[4:6]))
    
    # input must be matrix
    m<-as.matrix(timeseries_data[,-1])
    psd <- pwelch(m, fs = 1) # calculate welch power spectral density
    psd_sum<-apply(psd$spec, MARGIN = 2, FUN = sum)
    psd_sum<-c(sum(psd_sum[1:3]), sum(psd_sum[4:6]))
    
    temp<-c(abs_energy, std_dev, max_abs_amp, psd_sum)
    
    obs_move_summary<-c(obs_move_summary, temp)
  }
  move_all_data[i,]<-obs_move_summary
  
  # Questionnaire Data
  quest_data<-fromJSON(paste0(questionnaire, obs_q))
  answer<-as.vector(quest_data$item['answer'])  # extract answers of 30 questions
  answer<-unlist(answer)
  total_score<-sum(answer) # total score (number of TRUE)
  quest_all_data[i,]<-c(answer, total_score)

}

colnames(quest_all_data) <- c(paste0('Q', 1:30), 'total_score') # assign column name
colnames(move_all_data) <- all_columns # assign column name

####----Add questionnaire category information----
# Define the categories and their associated questions
categories <- list(
  Gastrointestinal_tract = c(1, 3, 4, 5, 6, 7),
  Urinal_tract = c(8, 9),
  Pain = c(10),
  Miscellaneous = c(11, 28, 29),
  Apathy_Attention_Memory = c(12, 13, 15),
  Distortion_of_perception = c(2, 14, 30),
  Depression_Anxiety = c(16, 17),
  Sexual_Function = c(18, 19),
  Cardiovascular = c(20, 21, 27),
  Sleep_Fatigue = c(22, 23, 24, 25, 26)
)

# Step 1: Initialize an empty 10 x 469 matrix
category_scores <- matrix(0, nrow = 469, ncol = length(categories))
colnames(category_scores) <- names(categories)

# Step 2: Populate the category scores matrix
for (category_name in names(categories)) {
  # Extract the question indices for the current category
  question_indices <- categories[[category_name]]
  
  # If the category has only one question, just copy the column
  if (length(question_indices) == 1) {
    category_scores[, category_name] <- quest_all_data[, question_indices]
  } else {
    # Otherwise, compute the sum of the scores for the questions in this category
    category_scores[, category_name] <- rowSums(quest_all_data[, question_indices])
  }
}

# Step 3: View the resulting category scores matrix
print(category_scores)

# Step 4: Concatenate with quest_all_data data
quest_final<-cbind(quest_all_data, category_scores)

####----save data to csv----
write.csv(quest_final, './questionnaire_preprocess.csv', row.names = F)
write.csv(move_all_data, './movement_preprocess.csv', row.names = F)
