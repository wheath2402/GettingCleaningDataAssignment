Merged dataset includes the variables (descriptions below):
Subject  
Activity  
Time.BodyAcc.Mean.X  
Time.BodyAcc.Mean.Y  
Time.BodyAcc.Mean.Z  
Time.BodyAcc.Std.X  
Time.BodyAcc.Std.Y  
Time.BodyAcc.Std.Z  
Time.GravityAcc.Mean.X  
Time.GravityAcc.Mean.Y  
Time.GravityAcc.Mean.Z  
Time.GravityAcc.Std.X  
Time.GravityAcc.Std.Y  
Time.GravityAcc.Std.Z  
Time.BodyAccJerk.Mean.X  
Time.BodyAccJerk.Mean.Y  
Time.BodyAccJerk.Mean.Z  
Time.BodyAccJerk.Std.X  
Time.BodyAccJerk.Std.Y  
Time.BodyAccJerk.Std.Z  
Time.BodyGyro.Mean.X  
Time.BodyGyro.Mean.Y  
Time.BodyGyro.Mean.Z  
Time.BodyGyro.Std.X  
Time.BodyGyro.Std.Y  
Time.BodyGyro.Std.Z  
Time.BodyGyroJerk.Mean.X  
Time.BodyGyroJerk.Mean.Y  
Time.BodyGyroJerk.Mean.Z  
Time.BodyGyroJerk.Std.X  
Time.BodyGyroJerk.Std.Y  
Time.BodyGyroJerk.Std.Z  
Time.BodyAccMag.Mean  
Time.BodyAccMag.Std  
Time.GravityAccMag.Mean  
Time.GravityAccMag.Std  
Time.BodyAccJerkMag.Mean  
Time.BodyAccJerkMag.Std  
Time.BodyGyroMag.Mean  
Time.BodyGyroMag.Std  
Time.BodyGyroJerkMag.Mean  
Time.BodyGyroJerkMag.Std  
Freq.BodyAcc.Mean.X  
Freq.BodyAcc.Mean.Y  
Freq.BodyAcc.Mean.Z  
Freq.BodyAcc.Std.X  
Freq.BodyAcc.Std.Y  
Freq.BodyAcc.Std.Z  
Freq.BodyAccJerk.Mean.X  
Freq.BodyAccJerk.Mean.Y  
Freq.BodyAccJerk.Mean.Z  
Freq.BodyAccJerk.Std.X  
Freq.BodyAccJerk.Std.Y  
Freq.BodyAccJerk.Std.Z  
Freq.BodyGyro.Mean.X  
Freq.BodyGyro.Mean.Y  
Freq.BodyGyro.Mean.Z  
Freq.BodyGyro.Std.X  
Freq.BodyGyro.Std.Y  
Freq.BodyGyro.Std.Z  
Freq.BodyAccMag.Mean  
Freq.BodyAccMag.Std  
Freq.BodyBodyAccJerkMag.Mean  
Freq.BodyBodyAccJerkMag.Std  
Freq.BodyBodyGyroMag.Mean  
Freq.BodyBodyGyroMag.Std  
Freq.BodyBodyGyroJerkMag.Mean  
Freq.BodyBodyGyroJerkMag.Std  

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 'Time' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (Time.BodyAcc.XYZ and Time.GravityAcc.XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (Time.BodyAccJerk.XYZ and Time.BodyGyroJerk.XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (Time.BodyAccMag, Time.GravityAccMag, Time.BodyAccJerkMag, Time.BodyGyroMag, Time.BodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing Freq.BodyAcc-XYZ, Freq.BodyAccJerk-XYZ, Freq.BodyGyro-XYZ, Freq.BodyAccJerkMag, Freq.BodyGyroMag, Freq.BodyGyroJerkMag. (Note the 'Freq.' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are: 
Mean: Mean value
Std: Standard deviation

The tidy dataset ('TidyData.txt') includes the mean of all variables grouped by Subject and Activity.