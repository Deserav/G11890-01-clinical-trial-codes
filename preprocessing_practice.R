# Load necessary libraries
library(jsonlite)
library(tibble)
library(dplyr)
library(ggplot2)
library(signal)  # For Welch's method
library(gsignal)

# load data practice
movement<-"C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/archive/movement/"
questionnaire<-"C:/Users/USER/Desktop/2024-2nd semester/clinical_trial/final_project/archive/questionnaire/"
obs<-'observation_001.json'
obs_q<-'questionnaire_response_001.json'

t1<-fromJSON(paste0(movement, obs))
t2<-fromJSON(paste0(questionnaire, obs_q))

# Example: obs1 - healthy / obs6 - PD
# Step 0: Let's see what the time series file looks like
time<-'timeseries/001_Relaxed_RightWrist.txt'
time_series<-read.table(paste0(movement, time), sep = ',', header = F,
                        col.names = c('time', 'acc_x', 'acc_y', 'acc_z', 'gy_x', 'gy_y', 'gy_z'))

# Convert time to numeric if needed
time_series$time <- as.numeric(time_series$time)

# summary statistics of each dataset
# Power
a<-pwelch(as.matrix(time_series[,-1]), fs = 1)
a$spec
apply(a$spec, MARGIN = 2, FUN = sum)

# standard deviation
# maximum altitude

# Plot 1: Accelerometer values (acc_x, acc_y, acc_z)
plot(time_series$time, time_series$acc_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values", ylim = c(-0.2, 0.2),
     main = "Accelerometer Values Over Time")
lines(time_series$time, time_series$acc_y, col = "blue4")
lines(time_series$time, time_series$acc_z, col = "green4")
legend("topright", legend = c("acc_x", "acc_y", "acc_z"), col = c("red4", "blue4", "green4"), lty = 1)

# Plot 2: Gyroscope values (gy_x, gy_y, gy_z)
plot(time_series$time, time_series$gy_x, type = "l", col = "red4", 
     xlab = "Time (minutes)", ylab = "Values",  ylim = c(-0.2, 0.2),
     main = "Gyroscope Values Over Time")
lines(time_series$time, time_series$gy_y, col = "blue4")
lines(time_series$time, time_series$gy_z, col = "green4")
legend("topright", legend = c("gy_x", "gy_y", "gy_z"), col = c("red4", "blue4", "green4"), lty = 1)

 y = "Power (dB)") +
  theme_minimal()

process_segments <- function(time_series) {
  segments <- split(time_series, cut(seq(nrow(time_series)), 4, labels = FALSE))
  segment_features <- lapply(segments, function(segment) {
    features <- sapply(segment[-1], function(col) {
      c(
        sd = sd(col),  # Standard deviation
        max_amp = max(abs(col)),  # Maximum absolute amplitude
        energy = sum(abs(col))  # Sum of absolute energy
      )
    })
    return(as.vector(features))
  })
  return(unlist(segment_features))
}
process_segments(time_series)




# Main Preprocessing Function
preprocess_data <- function(time_series, fs = 100) {
  # Step 1: PSD features
  psd_features <- process_psd(time_series, fs)
  
  # Step 2: Segment-based features
  segment_features <- process_segments(time_series)
  
  # Combine PSD and segment features
  return(c(unlist(psd_features), segment_features))
}


# Preprocess the time series data
preprocessed_features <- preprocess_data(time_series)
print(preprocessed_features)


# Step 1: Load the JSON file
json_file <- paste0(path, obs)  # Replace with your JSON file path
data <- fromJSON(json_file)

patients<-1:469

# Step 2: Parse and extract metadata and file names
extract_metadata <- function(json_data) {
  metadata <- list()
  for (session in json_data$session) {
    for (record in session$records) {
      metadata <- append(metadata, list(
        tibble(
          record_name = session$record_name,
          rows = session$rows,
          device_location = record$device_location,
          channels = list(record$channels),
          units = list(record$units),
          file_name = record$file_name
        )
      ))
    }
  }
  return(bind_rows(metadata))
}

metadata <- extract_metadata(data)

# Step 3: Load time series data from each file and attach it to metadata
load_and_attach_data <- function(metadata) {
  metadata_with_data <- metadata %>%
    rowwise() %>%
    mutate(
      time_series = list(
        read.table(file_name, header = FALSE, col.names = unlist(channels))
      )
    )
  return(metadata_with_data)
}

# Load the data
metadata_with_data <- load_and_attach_data(metadata)

# Step 4: Visualize the time series for a specific file
plot_time_series <- function(metadata_row) {
  time_series <- metadata_row$time_series[[1]]
  channels <- metadata_row$channels[[1]]
  device_location <- metadata_row$device_location
  record_name <- metadata_row$record_name
  
  # Melt data for plotting if there are multiple channels
  ts_long <- reshape2::melt(time_series, id.vars = "Time", variable.name = "Sensor", value.name = "Value")
  
  # Create the plot
  ggplot(ts_long, aes(x = Time, y = Value, color = Sensor)) +
    geom_line() +
    ggtitle(paste("Time Series:", record_name, "-", device_location)) +
    theme_minimal() +
    xlab("Time (s)") +
    ylab("Sensor Values")
}

# Example: Visualize the first record
plot_time_series(metadata_with_data[1, ])
