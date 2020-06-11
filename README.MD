Files are downloaded and unzipped into the working directory.

The variable names are read from features.txt and the preceding n) brackets are removed.

X_ measurement data is read in from either train or test, and variable names are attached to the data.

Activity data is read in and is converted to a factor to give descriptive activity names rather than 1-6.

Subject data is read in and converted to a numeric vector, enables easier arranging of tidy data down the line.

Activity, subject, and measurement data is column bound together producing a complete train or test dataframe. 

The test and train dataframes are row bound to produce large merged dataframe.

Mean and std variables are extracted using a regular expression. Variable names are then cleaned up using a series of regular expression substitutions. '-' are converted to '.', and title case is applied.

The large initial dataframe is now complete.

New tidy dataframe is created. Dataframe is grouped by subject and activity. The full dataset is then summarised by mean, and the result is arranged in ascending order of subject. 

New tidy data table is written and saved. 

scripts.R contains scripts to download data and convert all the way to the final tidy dataset.  
Codebook.MD contains descriptions of variables.
TidyData.txt contains final tidy data of means by subject and activity.