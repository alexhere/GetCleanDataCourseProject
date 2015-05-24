library(stringr)
library(readr)

readDataWithDblSpace <- function(fileName){
  # read file as character
  raw <- readChar(fileName,file.info(fileName)$size)
  
  # substitute all double spaces with single space
  raw <- str_replace_all(raw,fixed("  ")," ")
  
  # parse character into table
  dt <- read_delim(raw,delim = " ", col_names = FALSE, progress = FALSE)
  
  # remove 1st column (empty column)
  dt <- dt[,-1]
  dt
}
loadXData <- function(datafiles){
  # call readDataWithDblSpace for each filename provided
  dts <- lapply(datafiles, readDataWithDblSpace)
  
  # union data frames from all files into a single data frame
  dt <- do.call(rbind, dts)
  dt
}
loadXColNames <- function(file){
  # read from file
  Xlbl <- read_delim(file, delim = " ", col_names = FALSE, progress = FALSE)
  
  # return the 2nd column as vector
  Xlbl[[2]]
}
loadVectorData <- function(datafiles){
  # read all the files provided
  vecs <- lapply(datafiles, read_lines)
  
  # concatenate vectors from all files into a single vector
  vec <- do.call(c, vecs)
  vec
}

### Step 1 - Merges the training and the test sets to create one data set.

# load X data into dt
dt <- loadXData(c("UCI HAR Dataset/test/X_test.txt","UCI HAR Dataset/train/X_train.txt"))

# load column names for X
colNames <- loadXColNames("UCI HAR Dataset/features.txt")

# apply column names for X to dt
names(dt) <- colNames

# load activity (y) data and add it to dt
dt$Activity <- loadVectorData(c("UCI HAR Dataset/test/y_test.txt","UCI HAR Dataset/train/y_train.txt"))

# load subject data and add it to dt
dt$Subject <- loadVectorData(c("UCI HAR Dataset/test/subject_test.txt","UCI HAR Dataset/train/subject_train.txt"))

# reorder columns - move subject to the 1st column and activity to 2nd column
dt <- dt[,c("Subject", "Activity", colNames)]

# clean up object(s) no longer in use from environment
remove(colNames)

print("Step 1 Completed.")

### Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.

# create column numbers to column names data.frame, dtnames
dtnames <- data.frame(seq(from = 1,to = ncol(dt), by = 1), names(dt), stringsAsFactors = FALSE)

# rename dtnames columns
names(dtnames) <- c("Index", "Name")

# select only those records that meet the one of the following conditions and store in filteredNames 
# - Name contains "mean()"
# - Name contains "std()"
# - Name is "Activity"
# - Name is "Subject"
library(sqldf)
filteredNames <- sqldf("SELECT * FROM dtnames WHERE Name LIKE '%mean()%' OR Name LIKE '%std()%' OR Name = 'Activity' OR Name = 'Subject'")

# remove columns that does not exist in filteredNames from dt
dt <- dt[,filteredNames[["Index"]]]

# clean up object(s) no longer in use from environment
remove(dtnames, filteredNames)

print("Step 2 Completed.")

### Step 3 - Uses descriptive activity names to name the activities in the data set

# load activity labels
activity_labels <- read_delim("UCI HAR Dataset/activity_labels.txt",delim = " ", col_names = FALSE, progress = FALSE)

# apply activity labels to dt$Activity by converting it to factor
dt$Activity <- factor(dt$Activity, levels = activity_labels[[1]], labels = activity_labels[[2]])

# clean up object(s) no longer in use from environment
remove(activity_labels)

print("Step 3 Completed.")

### Step 4 - Appropriately labels the data set with descriptive variable names.

## START - Replace various terms in names with the corresponding descriptive terms
newNames <- names(dt)
newNames <- str_replace_all(newNames,fixed("mean()"),"Mean.Value")
newNames <- str_replace_all(newNames,fixed("std()"),"Standard.Deviation")

newNames <- str_replace_all(newNames,fixed("tBodyAcc-"),"Time.Domain_Body.Linear.Acceleration.Signal#")
newNames <- str_replace_all(newNames,fixed("tGravityAcc-"),"Time.Domain_Gravity.Acceleration.Signal#")
newNames <- str_replace_all(newNames,fixed("tBodyAccJerk-"),"Time.Domain_Body.Linear.Acceleration.Jerk.Signal#")
newNames <- str_replace_all(newNames,fixed("tBodyGyro-"),"Time.Domain_Body.Angular.Velocity.Signal#")
newNames <- str_replace_all(newNames,fixed("tBodyGyroJerk-"),"Time.Domain_Body.Angular.Velocity.Jerk.Signal#")

newNames <- str_replace_all(newNames,fixed("tBodyAccMag-"),"Time.Domain_Body.Linear.Acceleration.Magnitude#")
newNames <- str_replace_all(newNames,fixed("tGravityAccMag-"),"Time.Domain_Gravity.Acceleration.Magnitude#")
newNames <- str_replace_all(newNames,fixed("tBodyAccJerkMag-"),"Time.Domain_Body.Linear.Acceleration.Jerk.Magnitude#")
newNames <- str_replace_all(newNames,fixed("tBodyGyroMag-"),"Time.Domain_Body.Angular.Velocity.Magnitude#")
newNames <- str_replace_all(newNames,fixed("tBodyGyroJerkMag-"),"Time.Domain_Body.Angular.Velocity.Jerk.Magnitude#")

newNames <- str_replace_all(newNames,fixed("fBodyAcc-"),"Freq.Domain_Body.Linear.Acceleration.Signal#")
newNames <- str_replace_all(newNames,fixed("fBodyAccJerk-"),"Freq.Domain_Body.Linear.Acceleration.Jerk.Signal#")
newNames <- str_replace_all(newNames,fixed("fBodyGyro-"),"Freq.Domain_Body.Angular.Velocity.Signal#")

newNames <- str_replace_all(newNames,fixed("fBodyAccMag-"),"Freq.Domain_Body.Linear.Acceleration.Magnitude#")
newNames <- str_replace_all(newNames,fixed("fBodyBodyAccJerkMag-"),"Freq.Domain_Body.Linear.Acceleration.Jerk.Magnitude#")
newNames <- str_replace_all(newNames,fixed("fBodyBodyGyroMag-"),"Freq.Domain_Body.Angular.Velocity.Magnitude#")
newNames <- str_replace_all(newNames,fixed("fBodyBodyGyroJerkMag-"),"Freq.Domain_Body.Angular.Velocity.Jerk.Magnitude#")

## END - Replace various terms in names with corresponding descriptive terms

## START - Reorder terms in names
newNames <- str_replace_all(newNames,fixed("-X"),"#X")
newNames <- str_replace_all(newNames,fixed("-Y"),"#Y")
newNames <- str_replace_all(newNames,fixed("-Z"),"#Z")

sNames <- str_split_fixed(newNames,fixed("#"),3)

for(i in 1:nrow(sNames)){
  if(sNames[i,3] != ""){
    newNames[i] <- paste(sNames[i,1],sNames[i,3],sNames[i,2],sep = "_")
  }else if(sNames[i,2] != ""){
    newNames[i] <- paste(sNames[i,1],sNames[i,2],sep = "_")
  }else{
    newNames[i] <- sNames[i,1]
  }
}
## END - Reorder terms in names for better readability

# apply descriptive names to dt
names(dt) <- newNames

# clean up object(s) no longer in use from environment
remove(i, sNames, newNames)

print("Step 4 Completed.")

### Step 5 - From the data set in step 4, creates a second, independent tidy data set 
### with the average of each variable for each activity and each subject

library(dplyr)
tidydt <- dt %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))

print("Step 5 Completed.")

### Save tidydt to txt file
write.table(tidydt, "tidydata.txt",row.names = FALSE)

print("Saved to text file.")
