## 1) Merges the training and the test sets to create one data set.

## Get training data set

data.train.x <- read.table("UCIHARDataset/train/X_train.txt")
data.train.y <- read.table("UCIHARDataset/train/y_train.txt")
names(data.train.y) <- "Activity"
data.train.subject <- read.table("UCIHARDataset/train/subject_train.txt")
names(data.train.subject) <- "Subject"
data.train <- cbind(data.train.x, data.train.y, data.train.subject)

## Get test data set

data.test.x <- read.table("UCIHARDataset/test/X_test.txt")
data.test.y <- read.table("UCIHARDataset/test/y_test.txt")
names(data.test.y) <- "Activity"
data.test.subject <- read.table("UCIHARDataset/test/subject_test.txt")
names(data.test.subject) <- "Subject"
data.test <- cbind(data.test.x, data.test.y, data.test.subject)

data <- rbind(data.train, data.test)

## 2) Extracts only the measurements on the mean and standard deviation for each
## measurement.

features <- read.table("UCIHARDataset/features.txt")
features.selected <- grep("mean()|std()", features$V2)
features.selected <- append(features.selected,
                            c(which(colnames(data) == "Activity"),
                              which(colnames(data) == "Subject")))
data <- data[, features.selected]

## 3) Uses descriptive activity names to name the activities in the data set

activity.labels <- read.table("UCIHARDataset/activity_labels.txt")
data$Activity <- activity.labels[data$Activity,2]

## 4) Appropriately labels the data set with descriptive variable names.

features.names <- grep("mean()|std()", features$V2, value = TRUE)
features.names <- gsub("[-()]", "", features.names)
names(data) <- append(features.names, c("Activity", "Subject"))

## 5) From the data set in step 4, creates a second, independent tidy data set
## with the average of each variable for each activity and each subject.

data.final <- aggregate(data[, features.names],
                        by = list(Activity = data$Activity,
                                  Subject = data$Subject), mean)
write.table(data.final, "data_final.txt", row.names = FALSE)