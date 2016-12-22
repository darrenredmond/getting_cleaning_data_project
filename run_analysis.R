# set the working directory.
setwd("~/dev/coursera/johns_hopkins/getting_cleaning_data_project")

# read in the feature
feature_data <- read.table('UCI HAR Dataset/features.txt')

# read in the activity data
activity_data <- read.table('UCI HAR Dataset/activity_labels.txt')
names(activity_data) <- c('y', 'activity')

# read in the test data
x_test_data <- read.table('UCI HAR Dataset/test/X_test.txt')

y_test_data <- read.table('UCI HAR Dataset/test/y_test.txt')
names(y_test_data)[1] <- 'y'

subject_test_data <- read.table('UCI HAR Dataset/test/subject_test.txt')
names(subject_test_data)[1] <- 'subject'

# read in the train data
x_train_data <- read.table('UCI HAR Dataset/train/X_train.txt')

# 4. Appropriately labels the data set with descriptive variable names.
# set the names of x to be the features
names(x_test_data) <- feature_data$V2
names(x_train_data) <- feature_data$V2

y_train_data <- read.table('UCI HAR Dataset/train/y_train.txt')
names(y_train_data)[1] <- 'y'
subject_train_data <- read.table('UCI HAR Dataset/train/subject_train.txt')
names(subject_train_data)[1] <- 'subject'

# combine the columns for test and train data
test_data <- cbind(x_test_data, y_test_data, subject_test_data)
train_data <- cbind(x_train_data, y_train_data, subject_train_data)

# 1. Merges the training and the test sets to create one data set.
# combine the row data from test and train data sets
full_data <- rbind(test_data, train_data)

# play with extracting mean and std column names.
has_mean <- function(v) {
  grepl('mean', v)
}
has_std <- function(v) {
  grepl('std', v)
}
valid_column <- function(v) {
  has_mean(v) || has_std(v)  
}

matching_columns <- Filter(valid_column, names(full_data))

names(full_data)

# create the mean and standard deviation data variables for all columns with mean/std in it.
mean_std_data <- full_data[,grepl("mean", colnames(full_data)) | grepl("std", colnames(full_data))]

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_std_matrix <- rbind(apply(full_data, 2, mean), apply(full_data, 2, sd))
rownames(mean_std_matrix) <- c('mean', 'sd')

mean_std_matrix

# 3. Uses descriptive activity names to name the activities in the data set
names(full_data)
full_data <- mutate(full_data, activity_id = y)
full_data_activity <- merge(full_data, activity_data, by.x="y", by.y="y")

head(full_data_activity)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#    for each activity and each subject.

grouped_average_data <- full_data_activity %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

View(grouped_average_data)
