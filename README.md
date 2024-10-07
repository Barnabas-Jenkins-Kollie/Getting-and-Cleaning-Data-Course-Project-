# Peer-graded-Assignment-Getting-and-Cleaning-Data-Course-Project

The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set.

Review criteria
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.

Getting and Cleaning Data Course Project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

--------------------------------------------------------------------------------------------

From Question1: Please upload the tidy data set created in step 5 of the instructions. Please upload your data set as a txt file created with write.table() using row.name=FALSE (do not cut and paste a dataset directly into the text box, as this may cause errors saving your submission).

From Evaluation: Has the student submitted a tidy data set? Either a wide or a long form of the data is acceptable if it meets the tidy data principles of week 1 (Each variable you measure should be in one column, Each different observation of that variable should be in a different row).

From Question2: Please submit a link to a Github repo with the code for performing your analysis. The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory. The output should be the tidy data set you submitted for part 1. You should include a README.md in the repo describing how the script works and the code book describing the variables.

From Evaluation2: Github Repo with Required Scripts

Code Book From Overview: A code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md.

README I have included a README.md in the repo with my scripts. This repo explains how all of the scripts work and how they are connected.

##For 1st tiny data set: Read data sets and combine them Read subjects and combine them Read data labels and combine them Read features list Subset only only std and mean features from list Perform same subset on data set Rename features to be more human readable Read activity list Rename activities to be more human readable Rename data labels with activity name Merge data, subjects, and labels to single tiny data set Write tiny data set to file ##For 2nd tiny data set: average of measurement for activity and subject Prepare empty data set of appropriate length for Loop through subjects, then subloop through activities For each activity in a subject, get the full list of measurements Calculate the mean of each of these activities Place the means in subsequent columns of the subject/activity row Write second tiny data set to file.

--------------------------------------------------------------------------------------------------------------------

Obtain the raw data sets and put them in the working directory (via Rstudio)
The following steps were performed in a PC running the operation system Window 8.1. The data cleaning processes were performed in Rstudio with R version 3.1.0

download the raw data from the following website: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

using the following R command to download the data:


     >url_proj <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     >download.file(urlproj, destfile="Dataset")
unzip the raw data sets

     >unzip("Dataset.zip")
put the raw data sets in the selected working directory called "datacleaningproject" that is same name as in a repo in my Github account. (In other words, my working directory: "C:/Users/user/Documents/Getting-and-Cleaning-Data-Course-Project-")

     > getwd()

    [1] "C:/Users/user/Documents/Getting-and-Cleaning-Data-Course-Project-"
Create a tidy data via a R script called run_analysis.R
Preparation: data sets and script

The data sets and run_analysis.R must be in the working directory. It is based on one of the requirements this project: The code should have a file run_analysis.R in the main directory that can be run as long as the Samsung data is in your working directory. (see "Instructions and Requirements")

The input raw data for the run_analys.R are:

    ./train/X_train.txt, ./train/y_train.txt, subject_train.txt;
    ./test/X_test.txt, ./test/y_test.txt,  subjecct_test.txt;
    ./activity_labels.txt, ./features.txt
The output tidy data created from run_analysis.R are:

    ./tidy_average_data.txt (180 rows)
    ./combinedcleaningdata.txt (optional)
How the script run_analysis.R works via Rstudio

usage:

     > source("run_analysis.R") ## load the script
     > run_analysis() ##run the script


Step-1: Merges the training and the test data sets to create one data set (such as): Load and read the input raw sets; merge three pairs data set (e.g.: X_train.txt, X_test.txt; y_train.txt, y_test.txt; subject_train.txt, subject_text.txt) into three single data set via "rbind" function.

Setp-2: Extracts only the measurements on the mean and standard deviation for each measurement. This step is mainly done by the "grep" function by providing the key search patterns "-mean\(\)|-std\(\)".

    ++ column names from the data sets were changed into lower-case to avoid any uncessary typos
     (based on the intruction from the "Week4 slide-notes: Editing Text Variables" from 
     Coursera course Getting and Cleaning Data); characters "()" were replaced "" and
     characters "-" was replaced to "." via "gsub" function.
Step-3: Uses descriptive activity names to name the activities in the data set. This step is mainly to produce a one-column data frame called "joinlabel" containing descriptive activity names.

Step-4: Appropriately labels the data set with descriptive activity names. The step is mainly to combine three joined data frames into one data frame called "cleandata". This is the first cleaned data frame toward to the final tidy data set. An (optional/temporary) output file is created called "combinedcleandata.txt" just in case for an emergency.

Step-5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

    + It is necessary to install and to load the package "reshape2" before to perform
    any processes.
    
    + The final tidy data  called "tidy_data".
  
    + Finally, a file called "tidyDataSet.txt" was created via "write.table" function.
      This is the final output tidy processed data.
    
    + The detailed description of the "tidyDataSet.txt" and the given raw data set
