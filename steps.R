##workings
library(dplyr)

##download and unzip files
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "datafile.zip")
unzip("datafile.zip")

##features read in
features <- readLines("UCI HAR Dataset/features.txt")
features <- sub("[0-9]+ ","",features,)

##data read in
X_test <- read.table("UCI HAR Dataset/test/X_test.txt")

##names changed
names(X_test) <- features

##labels read and converted to factor
Activity <- readLines("UCI HAR Dataset/test/y_test.txt")
Activity <- factor(Activity, levels = c(1, 2, 3, 4, 5, 6),
                      labels = c("WALKING", "WALKING_UPSTAIRS", 
                                 "WALKING_DOWNSTAIRS", "SITTING", 
                                 "STANDING", "LAYING"))

#subject read and numeric
Subject <- as.numeric(readLines("UCI HAR Dataset/test/subject_test.txt"))

#make type
Type <- rep("TEST", nrow(X_test))

##cbind all test columns
TestDF <- cbind(Subject, Activity, Type, X_test)

##repeated for Train
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
names(X_train) <- features
Activity <- readLines("UCI HAR Dataset/train/y_train.txt")
Activity <- factor(Activity, levels = c(1, 2, 3, 4, 5, 6),
                labels = c("WALKING", "WALKING_UPSTAIRS", 
                           "WALKING_DOWNSTAIRS", "SITTING", 
                           "STANDING", "LAYING"))
Subject <- as.numeric(readLines("UCI HAR Dataset/train/subject_train.txt"))
Type <- rep("TRAIN", nrow(X_train))
TrainDF <- cbind(Subject, Activity, Type, X_train)
##two dfs TestDF & TrainDF now present

##rbind them
InitialDF <- rbind(TrainDF, TestDF)

##extract mean and std cols
columns <- names(InitialDF)
logic <- grepl("(.*[Ss]td\\(\\).*|.*[Mm]ean\\(\\).*)|(Subject|Activity)", columns)
InitialDF <- InitialDF[,logic]

##column names tidied
RawNames <- names(InitialDF)
UpNames <- gsub("-",".", RawNames);
UpNames <- gsub("\\(\\)","", UpNames)
UpNames <- gsub("mean", "Mean", UpNames)
UpNames <- gsub("std", "Std", UpNames)
UpNames <- gsub("^t", "Time.", UpNames)
UpNames <- gsub("^f", "Freq.", UpNames)
names(InitialDF) <- UpNames


##make second tidy set
TidyData <- InitialDF %>%
        group_by(Subject, Activity) %>%
        summarise_all(funs(mean)) %>%
        arrange(Subject, Activity)
write.table(TidyData, "TidyData.txt", row.name=FALSE)
