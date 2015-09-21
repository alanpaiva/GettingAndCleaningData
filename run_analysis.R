# Step1 - Merges the training and the test sets to create one data set.
trainData <- read.table("./dataset/train/X_train.txt")
dim(trainData)
head(trainData)
trainLabel <- read.table("./dataset/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./dataset/train/subject_train.txt")
testData <- read.table("./dataset/test/X_test.txt")
dim(testData)
testLabel <- read.table("./dataset/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./dataset/test/subject_test.txt")
joinData <- rbind(trainData, testData)
dim(joinData)
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel)
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject)

# Step2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
features <- read.table("./dataset/features.txt")
dim(features)
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices)
joinData <- joinData[, meanStdIndices]
dim(joinData)
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
names(joinData) <- gsub("mean", "Mean", names(joinData))
names(joinData) <- gsub("std", "Std", names(joinData))
names(joinData) <- gsub("-", "", names(joinData)) 

# Step3 - Uses descriptive activity names to name the activities in the data set
activity <- read.table("./dataset/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"

# Step4 - Appropriately labels the data set with descriptive activity names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData)
write.table(cleanedData, "merged_data.txt")

# Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1]
columnLen <- dim(cleanedData)[2]
res <- matrix(NA, nrows=subjectLen*activityLen, ncol=columnLen) 
res <- as.data.frame(res)
colnames(res) <- colnames(cleanedData)
rows <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    res[rows, 1] <- sort(unique(joinSubject)[, 1])[i]
    res[rows, 2] <- activity[j, 2]
    b1 <- i == cleanedData$subject
    b2 <- activity[j, 2] == cleanedData$activity
    res[rows, 3:columnLen] <- colMeans(cleanedData[b1&b2, 3:columnLen])
    rows <- rows + 1
  }
}
head(res)
write.table(res, "data_with_means.txt")

data <- read.table("./data_with_means.txt")
data[1:12, 1:3]
