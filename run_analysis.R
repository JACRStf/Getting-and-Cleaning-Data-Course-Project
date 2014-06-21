library(data.table)

#Step 1: "Merges the training and the test sets to create one data set."
# Read the data
# Set working directory for files ZIP location
setwd("~/Documents/CourseraWorkingFiles/R Programing/Geting and Cleansing Data/UCI HAR Dataset")

# Get the features
feature.tble <- read.table('./features.txt', col.names = c('index', 'name')) #imports features.txt

#Step 2: "Extracts only the measurements on the mean and standard deviation for each measurement."
features <- subset(feature.tble, grepl('-(mean|std)[(]', feature.tble$name)) 
#Step 3: "Uses descriptive activity names to name the activities in the data set."
# Get the labels
label.tble <- read.table('./activity_labels.txt', col.names = c('level', 'label')) #imports activity_labels.txt

# Read in the train data
train.dtaset <- crt.dtaset('train', features, label.tble) 
# Read in the test data
test.dtaset <- crt.dtaset('test', features, label.tble) 

# Generate a data set with the Merge of training and test sets.
dtaset <- rbind(train.dtaset, test.dtaset)

# Generate the tidy data set
tidy.dtaset <- dtaset[, lapply(.SD, mean), by=list(label, subject)]

#Step 4: "Appropriately labels the data set with descriptive activity names." 
# Fix the variable names
names <- names(tidy.dtaset)
names <- gsub('-mean', 'Mean', names) # Replace `-mean' by `Mean'
names <- gsub('-std', 'Std', names) # Replace `-std' by 'Std'
names <- gsub('[()-]', '', names) # Remove the parenthesis and dashes
names <- gsub('BodyBody', 'Body', names) # Replace `BodyBody' by `Body'
setnames(tidy.dtaset, names)

# Write the raw and the tidy data sets to files
setwd('..')
write.csv(dtaset, file = 'rawdata.csv', row.names = FALSE)
#Step 5: "Creates a second, independent tidy data set with the average of each variable for each activity and each subject."
write.csv(tidy.dtaset, file = 'tidydata.csv', row.names = FALSE, quote = FALSE)


# The function loads and processes either a train or a test data set
crt.dtaset <- function (set, features, labels) {
  # Construct the relative pathes of data files
  prefix <- paste(set, '/', sep = '')
  file.data <- paste(prefix, 'X_', set, '.txt', sep = '')
  file.label <- paste(prefix, 'y_', set, '.txt', sep = '')
  file.subject <- paste(prefix, 'subject_', set, '.txt', sep = '')
  
  # read the data into a data.frame
  data <- read.table(file.data)[, features$index]
  names(data) <- features$name
  
  label.set <- read.table(file.label)[, 1]
  data$label <- factor(label.set, levels=labels$level, labels=labels$label)
  
  subject.set <- read.table(file.subject)[, 1]
  data$subject <- factor(subject.set)
  
  # then transform it into data.table
  data.table(data)
}
