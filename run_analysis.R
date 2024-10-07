
## run_analysis.R
## author: Jenkins J B Kollie
## 
## The purpose of this project is to demonstrate course students' ability to collect,
## work with, and clean a data set. The goal is to prepare tidy data that can be used
## for later analysis.
## This script is written for creating a tidy data file that can be used for later analysis.
## There are five main steps to fulfill the above goal.
##
## input files: x_train.txt,x_test.txt,y_train.txt,y_test.txt,subject_train.txt,subject_test.txt
##              features.txt, activity_labels.txt
## output files: tidy_average_data.txt, tidy_data.txt



## re-confirm the cleaning process has been performed and a tidy data file

run_analysis <- function() {
  
  
  ## Three pairs of data (training and test data sets) need to be merged together
  ## They are: X_train.txt, X_test.txt; (data set)
  ##           y_train.txt, y_test.txt; (label-data set)
  ##           subject_train.txt, subject_test.txt (subject-data set)
  
  ##Load libraries to perform proper analysis. 
  # I am using tidyr and dplyr instead of Reshape, because reshape is depreciated.
  library(dplyr)
  library(tidyr)
  
  ## load/read the data sets and then merge them
  ####### read and merge data set pair##############
  traindata_x <- read.table(file = "UCI HAR Dataset/train/X_train.txt")
  testdata_x <- read.table(file = "UCI HAR Dataset/test/X_test.txt")
  joined_data  <- rbind(traindata_x, testdata_x) 
  
  ## cross-check dimensions: (observations/rows, variables/columns)
  dim(traindata_x) ##  (7352, 561)
  dim(testdata_x)  ##  (2947, 561)
  dim(joined_data)  ## (10299, 561)
  
  ####### read and merge label-data set pair ##############################
  trainlabel <- read.table("UCI HAR Dataset/train/y_train.txt")
  testlabel  <- read.table("UCI HAR Dataset/test/y_test.txt")
  joinlabel  <- rbind(trainlabel, testlabel)
  
  ## cross-check dimensions: (observations/rows, variables/columns)
  dim(trainlabel) ## (7352, 1)
  dim(testlabel)  ## (2947, 1)
  dim(joinlabel)  ##(10299, 1)
  
  ### read and merge subject-data set pair #################################
  testsubject <- read.table(file = "UCI HAR Dataset/test/subject_test.txt")
  trainsubject  <- read.table(file = "UCI HAR Dataset/train/subject_train.txt")
  joinsubject  <- rbind(trainsubject, testsubject)
  
  ##  cross-check dimensions: meanStdIndex <- grep("-mean\\(\\) | -std\\(\\)", features[, 2])
  dim(trainsubject) ##  (7352, 1)
  dim(testsubject)  ##  (2947, 1)
  dim(joinsubject)  ## (10299, 1)
  
  #########################################################################################
  ## Step 2: Extracts only the measurements on the mean and standard deviation for 
  ##      each measurement.
  ##
  
  
  cat("Step2: Extracts only the measurements on the mean and standard deviation for each measurement.\n")
  ##
  ### In order to extract the measurements on the mean and standard deviation,
  ##  I need locate variable names (column names) related to "mean" and
  ##  "standard deviation" stored in the file "features.txt" via the command grep
  
  features <- read.table(file = "UCI HAR Dataset/features.txt") ##read/load the file
  ## cross-check dimensions: (rows, columns)
  dim(features) ## (561, 2)
  
  ## locate column names with "-mean()" or "-std()" in any rows in column 2
  meanstdindex <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
  
  ##cross-check length(meanstdindex)
  length(meanstdindex) ## 66 variables
  
  ## only select those columns/measurements on the mean and standard deviation
  ## make a new data frame called joindatanew
  joindatanew <- joined_data[, meanstdindex] 
  
  ## cross-check dimensions: (rows, columns)
  dim(joindatanew)  ##(10299,66)
  
  ## put the column names into the new data frame joindatanew
  colnames(joindatanew) <- features[meanstdindex, 2] 
  
  ## remove the bad characters such as "()", "-" in colnames, also lower the case if possible
  ##   in order to avoid any unnecessary errors in later analysis
  ##
  colnames(joindatanew) <- gsub("\\(|\\)", "", colnames(joindatanew)) 
  colnames(joindatanew) <- gsub("-", ".", colnames(joindatanew))
  colnames(joindatanew) <- tolower(colnames(joindatanew))
  
  #########################################################################################
  ## Step 3: Uses descriptive activity names to name the activities in the data set.
  
  cat("\n")
  cat("Step3: Uses descriptive activity names to name the activities in the data set.\n")
  
  ## Firstly, it is necessary to load/read the file containing full activity names
  activity <- read.table(file = "UCI HAR Dataset/activity_labels.txt")
  
  ## Remove bad characters such as "_" and also lower case in the activity names/row names
  activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
  
  ## Create a new activitylabel vector containing descriptive activity names with
  ## the length of rows of joinlabel[, 1] 
  activitylabel <- activity[joinlabel[, 1], 2]
  
  ## Replace the 'activity numbers' to descriptive activity names in joinlabel data frame
  joinlabel[, 1] <- activitylabel 
  
  ## Give a column name to the column in the joinlabel data frame (one column data frame)
  colnames(joinlabel) <- "activity"
  
  #########################################################################################
  ## Step 4: Appropriately labels the data set with descriptive activity names.
  ##      (i.e.: create a "clean" data set with labels (colnames and rownames))
  
  cat("\n")
  cat("Step4: Appropriately labels the data set with descriptiv activity names.\n")
  
  ##  Firstly, give a column name to the column of the joinsubject data frame (one column data frame)
  colnames(joinsubject) <- "subject"
  
  
  ##  Combine three working dataframes (joinsubject, joinlabel and joindatanew) into one 
  ##  single data frame via command cbind
  
  cleandata <- cbind(joinsubject, joinlabel, joindatanew)
  
  ## Cross-check dimensions: (nrows, ncolumns)
  dim(cleandata) ## (10299    68)
  
  
  
  # gather all the data base on the average of the activity and subject
  tidy_data <- cleandata %>%
    group_by(activity, subject) %>%
    summarize_all(mean) 

  #created a view to know things works fine
  View(tidy_data)
    
    
  ## Create a file containing the tidy data set
  write.table(tidy_data, "tidy_data.txt", row.names = F, col.names= T, sep = "\t")
  
} 










