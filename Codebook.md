Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.
Dataset Characteristics

Dataset Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

Variables:

## Activity

there are six activities an individual must performed
WALKING,
WALKING_UPSTAIRS,
WALKING_DOWNSTAIRS,
SITTING,
STANDING,
LAYING

Subject
has values from 1 - 30

all these are columns with numeric data.
the data are group in terms of mean and standard deviation

tbodyacc.mean.x
tbodyacc.mean.y
tbodyacc.mean.z
tbodyacc.std.x
tbodyacc.std.y  
tbodyacc.std.z

tgravityacc.mean.x
tgravityacc.mean.y
tgravityacc.mean.z  
 tgravityacc.std.x  
 tgravityacc.std.y
tgravityacc.std.z

tbodyaccjerk.mean.x
tbodyaccjerk.mean.y
tbodyaccjerk.mean.z
tbodyaccjerk.std.x  
 tbodyaccjerk.std.y
tbodyaccjerk.std.z

tbodygyro.mean.x
tbodygyro.mean.y
tbodygyro.mean.z
tbodygyro.std.x
tbodygyro.std.y
tbodygyro.std.z

tbodygyrojerk.mean.x
tbodygyrojerk.mean.y
bodygyrojerk.mean.z  
 tbodygyrojerk.std.x
tbodygyrojerk.std.y
tbodygyrojerk.std.z

tbodyaccmag.mean  
 tbodyaccmag.std  
 tgravityaccmag.mean  
 tgravityaccmag.std
tbodyaccjerkmag.mean
tbodyaccjerkmag.std

tbodygyromag.mean
tbodygyromag.std
tbodygyrojerkmag.mean
tbodygyrojerkmag.std

fbodyacc.mean.x
fbodyacc.mean.y  
 fbodyacc.mean.z
fbodyacc.std.x
fbodyacc.std.y
fbodyacc.std.z

fbodyaccjerk.mean.x
fbodyaccjerk.mean.y
fbodyaccjerk.mean.z
fbodyaccjerk.std.x
fbodyaccjerk.std.y
fbodyaccjerk.std.z

fbodygyro.mean.x
fbodygyro.mean.y  
 fbodygyro.mean.z
fbodygyro.std.x
fbodygyro.std.y
fbodygyro.std.z

fbodyaccmag.mean  
 fbodyaccmag.std
fbodybodyaccjerkmag.mean
fbodybodyaccjerkmag.std  
 fbodybodygyromag.mean
fbodybodygyromag.std
fbodybodygyrojerkmag.mean
fbodybodygyrojerkmag.std
