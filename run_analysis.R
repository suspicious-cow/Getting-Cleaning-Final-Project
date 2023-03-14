
#################################################################################################################################
# WARNING:  This code is an original work from https://github.com/suspicious-cow/Getting-Cleaning-Final-Project                 #
#           If this work is submitted for course work and is not from this repository then it is most likely plagiarized        #
#################################################################################################################################

# check to see if the dplyr package is installed and the library is loaded
# if anything is missing, install and load as needed
print("Loading packages and libraries if needed...")
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
} else {
  library("dplyr")
}


# check to see if the folder already exists for our data and, if not, download
# and unzip the data
# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  # let the user know we are getting the file
  print("Setting up the raw data file(s) now...")
  
  # variable to hold our file name that we will use for analysis
  dataFileName <- "harData.zip"
  
  ## Download the file used for our analysis if we don't already have it
  if (!file.exists(dataFileName)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, dataFileName, method = "curl")
  }  
  
  # unzip the data to create our folder with the data in it we need
  unzip(dataFileName) 
}

# load all our dataframes and let the user know each stage of the process
print("Beginning dataframe loading process, please be patient as this may take a few minutes...")

print("Loading the features dataframe (1 of 8)")
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))

print("Loading the activities dataframe (2 of 8)")
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

print("Loading the subject_test dataframe (3 of 8)")
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")

print("Loading the x_test dataframe (4 of 8)")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)

print("Loading the y_test dataframe (5 of 8)")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")

print("Loading the subject_train dataframe (6 of 8)")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")

print("Loading the x_train dataframe (7 of 8)")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)

print("Loading the y_train dataframe (8 of 8)")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

# combine the training and test sets to create one dataframe and let the user
# know the process that is happening
print("Beginning combining all the data into one data frame")
xBind <- rbind(x_train, x_test)
yBind <- rbind(y_train, y_test)
print("Halfway done combining all the data into one data frame")
subjectBind <- rbind(subject_train, subject_test)
combinedData <- cbind(subjectBind, xBind, yBind)
print("Done combining all the data into one data frame")

# Begin building our clean data for end user consumption by extracting the mean
# and std deviation from the combined data
print("Beginning the cleaning process...")
cleanData <- combinedData %>% select(subject, code, contains("mean"), contains("std"))

# fix the code column in our clean data by replacing the codes with the actual
# activity names that are in our activities dataframe
cleanData$code <- activities[cleanData$code,2]

# Fix all the unclear variable names with more understandable and consistent names
print("Making column names more readable...")
names(cleanData)[1] = "Subject"
names(cleanData)[2] = "Activity"
names(cleanData)<-gsub("\\.", "", names(cleanData)) # get rid of all periods in the names
names(cleanData)<-gsub("Acc", "Accelerometer_", names(cleanData))
names(cleanData)<-gsub("Gyro", "Gyroscope_", names(cleanData))
names(cleanData)<-gsub("BodyBody", "Body_", names(cleanData))
names(cleanData)<-gsub("Body", "Body_", names(cleanData))
names(cleanData)<-gsub("Mag", "Magnitude_", names(cleanData))
names(cleanData)<-gsub("Time", "Time_", names(cleanData))
names(cleanData)<-gsub("^t", "Time_", names(cleanData))
names(cleanData)<-gsub("Freq", "Frequency_", names(cleanData))
names(cleanData)<-gsub("^f", "Frequency_", names(cleanData))
names(cleanData)<-gsub("tBody", "Time_Body_", names(cleanData))
names(cleanData)<-gsub("-mean()", "Mean_", names(cleanData), ignore.case = TRUE)
names(cleanData)<-gsub("mean", "Mean_", names(cleanData), ignore.case = TRUE)
names(cleanData)<-gsub("Jerk", "Jerk_", names(cleanData), ignore.case = TRUE)
names(cleanData)<-gsub("-std()", "STD_", names(cleanData), ignore.case = TRUE)
names(cleanData)<-gsub("-freq()", "Frequency_", names(cleanData), ignore.case = TRUE)
names(cleanData)<-gsub("angle", "Angle_", names(cleanData))
names(cleanData)<-gsub("[Gg]ravity", "Gravity_", names(cleanData))
names(cleanData)<-gsub("X", "X", names(cleanData))
names(cleanData)<-gsub("Y", "Y", names(cleanData))
names(cleanData)<-gsub("_$", "", names(cleanData)) # clean up any underscores at the end of the text

# clean up our variables that won't be used for analysis but remove them one by one
# so that users can opt to keep certain variables if they want to by commenting out
# any line of code
print("Clearing out extra up the environment variables...")
# remove(activities)
# remove(combinedData)
# remove(features)
# remove(subject_test)
# remove(subject_train)
# remove(subjectBind)
# remove(x_test)
# remove(x_train)
# remove(xBind)
# remove(y_test)
# remove(y_train)
# remove(yBind)

# Create a second, independent tidy data set with the average of each variable 
# for each activity and each subject
avgCleanDataset <- cleanData %>% 
  group_by(Subject, Activity) %>%
  summarize_all(list(mean))

# Let the user know about our two data frames and let them start their analysis
print("You now have two data frames to work with. \n The first one, called cleanData, is the original data made tidy")
print("The second one, called avgCleanData, contains the average of each variable for each activity and each subject")
print("These can now be manipulated as you see fit for analysis.")
