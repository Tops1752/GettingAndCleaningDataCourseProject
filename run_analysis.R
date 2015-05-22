## Step 1: Merges the training and the test sets to create one data set.

# read data
ActivityLabel <- read.table("./UCI HAR Dataset/activity_labels.txt")
Features      <- read.table("./UCI HAR Dataset/features.txt")

TrainingSet   <- read.table("./UCI HAR Dataset/train/X_train.txt")
TrainingLabel <- read.table("./UCI HAR Dataset/train/y_train.txt")
TrainingSub   <- read.table("./UCI HAR Dataset/train/subject_train.txt")

TestSet       <- read.table("./UCI HAR Dataset/test/X_test.txt")
TestLabel     <- read.table("./UCI HAR Dataset/test/y_test.txt")
TestSub       <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Merge data set and label
AllData <- rbind(TrainingSet,TestSet)
Label   <- rbind(TrainingLabel,TestLabel)
Subject <- rbind(TrainingSub,TestSub)



## Step 2: Extracts only the measurements on the mean and standard deviation for each measurement. 

# observe the data and find variable names corresponding to mean or standard deviation
selectCol <- grepl("-mean..",Features[,2]) | grepl("-std..",Features[,2])

# extracts data
selectedData <- AllData[selectCol == TRUE]
selectedName <- Features[,2][selectCol == TRUE]


## Step 3: Uses descriptive activity names to name the activities in the data set

# create another list with label description 
Activity <- sapply(Label[,1],function(x){ActivityLabel[as.numeric(x),2]})

# add label and subject column to data
DataSet <- cbind(selectedData,Label[,1],Activity,as.factor(Subject[,1]))


## Step 4: Appropriately labels the data set with descriptive variable names. 
DataName          <- c(as.vector(selectedName),"Label","Activity","Subject")
colnames(DataSet) <- DataName


## Step 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# create new data set without calculating mean of last columns which are label, activity and subject
factorIndex <- ncol(DataSet) - 2
tidyData<-aggregate(x=DataSet[,-(factorIndex:ncol(DataSet))],by=list(DataSet$Activity,DataSet$Subject),data=DataSet,FUN=mean)

# name two groups
colnames(tidyData)[1] <- "Activity"
colnames(tidyData)[2] <- "Subject"

# write data to file
write.table(tidyData,"./GettingAndCleaningDataCourseProject/tidy_data.txt",row.names = FALSE)

