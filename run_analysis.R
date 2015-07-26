library(plyr)
library(dplyr)

# Reading in test data.
testX <- read.table(
  "dataset/test/X_test.txt", 
  header=FALSE
)

# Reading in test activites
testY <- read.table(
  "dataset/test/Y_test.txt", 
  header=FALSE
)

# Reading in test participants
subjectTest <- read.table(
  "dataset/test/subject_test.txt", 
  header=FALSE
)

# Merge testing activities and data
testData <- cbind(testY, subjectTest, testX)

# Reading in training data.
trainX <- read.table(
  "dataset/train/X_train.txt", 
  header=FALSE
)

# Reading in training activities.
trainY <- read.table(
  "dataset/train/Y_train.txt", 
  header=FALSE
)

# Reading in test participants
subjectTrain <- read.table(
  "dataset/train/subject_train.txt", 
  header=FALSE
)

# Merge training activities and data
trainData <- cbind(trainY, subjectTrain, trainX)

# Merging both the merged training and testing sets.
dataMerged <- rbind(testData, trainData)

# Reading in features.
features <- read.table(
  "dataset/features.txt", 
  header=FALSE, 
  stringsAsFactors = FALSE
)

# This statement retrieves all rows which pertain to standard deviation or mean.
stdMeanFeatures <- filter(features, grepl("std|mean", V2))

# We do not want to include the meanFreq column simply because that is not the same
# thing as the mean.
colnames(dataMerged[1]) 
stdMeanFeatures <- filter(stdMeanFeatures, !grepl("meanFreq", V2))
addTwo <- lapply(stdMeanFeatures$V1, function(item) { item + 2 })
selectCols <- append(addTwo, 1:2, 0)

# Now we need to subset the main data set to only include columns from the stdMeanFeatures data.frame
colnames(dataMerged) <- c(1:563)
dataStdMean <- select(dataMerged, as.numeric(unlist(selectCols)))
colnames(dataStdMean)[1:2] <- c('activity', 'subject')

# We also want to update the activity column so we have the descriptive name.
# It's not entirely necessary to read in the activity_labels.txt file because the assignment doesn't require it.
# This is less work in the short term; however if the activity labels change, we'll need to update this, but that is unlikely
# since nothing has changed since 2012.
dataStdMean$activity[dataStdMean$activity==1] <- "WALKING"
dataStdMean$activity[dataStdMean$activity==2] <- "WALKING_UPSTAIRS"
dataStdMean$activity[dataStdMean$activity==3] <- "WALKING_DOWNSTAIRS"
dataStdMean$activity[dataStdMean$activity==4] <- "SITTING"
dataStdMean$activity[dataStdMean$activity==5] <- "STANDING"
dataStdMean$activity[dataStdMean$activity==6] <- "LAYING"

# We now need to label the columns for each activity.
colnames(dataStdMean)[c(3:68)] = stdMeanFeatures$V2

# Create averages data set.
averages <- melt(dataStdMean, id=c("activity", "subject"))

# Get mean.
averages <- dcast(averages, activity + subject ~ variable, mean)

# Create the tidy data set.
write.table(averages, "tidy.txt", row.name = FALSE)
