library(tidyverse)

#Download data
rawdata_dir <- "./raw_data"
external_rawdata_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
rawdata_filename  <- "rawdata.zip"
rawdate_location <- paste(c(rawdata_dir, "/", rawdata_filename), collapse = "")
data_dir <- "./data"
data_dir_prefix <- "UCI HAR Dataset"

if (!file.exists(rawdata_dir)) {
  dir.create(rawdata_dir)
  download.file(url = external_rawdata_url, destfile = rawdate_location)
}
if (!file.exists(data_dir)) {
  dir.create(data_dir)
  unzip(zipfile = rawdate_location, exdir = data_dir)
}

activity_labels <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/activity_labels.txt"),collapse = ""), col_names = c("activity_id","position"))
features <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/features.txt"),collapse = ""), col_names = c("id","functions"))

testdata_subject <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/test/subject_test.txt"),collapse = ""), col_names = c("subject_id"))
testdata_x <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/test/X_test.txt"),collapse = ""), col_names = features$functions, progress = show_progress())
testdata_y <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/test/y_test.txt"),collapse = ""), col_names = c("activity_id"))

trainingdata_subject <-read_table2(paste(c(data_dir,"/",data_dir_prefix,"/train/subject_train.txt"),collapse = ""), col_names = c("subject_id"))
trainingdata_x <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/train/X_train.txt"),collapse = ""), col_names = features$functions, progress = show_progress())
trainingdata_y <- read_table2(paste(c(data_dir,"/",data_dir_prefix,"/train/Y_train.txt"),collapse = ""), col_names = c("activity_id"))

#Merges the training and the test sets to create one data set.
testdata_merged <- bind_cols(testdata_subject,testdata_x,testdata_y)
trainingdata_merged <- bind_cols(trainingdata_subject,trainingdata_x,trainingdata_y)
merged_data <- bind_rows(testdata_merged,trainingdata_merged)

#Extracts only the measurements on the mean and standard deviation for each measurement.
compact_data <- merged_data %>%
                select(subject_id, activity_id, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set
compact_data <- compact_data %>% 
  rename_with(~ gsub('[-()]', '', .x)) %>%
  rename_with(~ gsub('mean', 'Mean', .x)) %>%
  rename_with(~ gsub('std', 'Std', .x)) %>%
  rename_with(~ gsub('BodyBody', 'Body', .x)) %>%
  # Appropriately labels the data set with descriptive variable names. 
  mutate(activity_id = factor(activity_id, levels = activity_labels$activity_id, labels = activity_labels$position)) %>%
  rename(activity = activity_id)

#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
mean_data <- compact_data %>%
  group_by(subject_id,activity) %>%
  summarise(across(everything(), list(mean = mean))) %>%
  rename_with(~ gsub('_mean', '', .x))

write.table(mean_data, "dataset.txt", row.name=FALSE)

