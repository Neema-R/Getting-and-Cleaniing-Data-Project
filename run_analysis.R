
library(reshape2)

> ## Merge the training and the test sets to create one data set.


> train_X <- read.table("train/X_train.txt")
> test_X <- read.table("test/X_test.txt")
> X <- rbind(train_X, test_X)
> 
> train_sub <- read.table("train/subject_train.txt")
> test_sub <- read.table("test/subject_test.txt")
> S <- rbind(train_sub, test_sub)
> 
> train_Y <- read.table("train/y_train.txt")
> test_Y <- read.table("test/y_test.txt")
> Y <- rbind(train_Y, test_Y)
> 
> ## Extracts only the measurements on the mean and standard deviation for each measurement.
> 
> features <- read.table("features.txt")
> imp_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
> X <- X[, imp_features]
> names(X) <- features[imp_features, 2]
> 
> names(X) <- gsub("\\(|\\)", "", names(X))
> names(X) <- tolower(names(X))
> 
> ## Uses descriptive activity names to name the activities in the data set
> 
> activities <- read.table("activity_labels.txt")
> activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
> Y[,1] = activities[Y[,1], 2]
> names(Y) <- "activity"
> 
> ## Appropriately labels the data set with descriptive activity names.
> 
> names(S) <- "subject"
> cleaned <- cbind(S, Y, X)
> write.table(cleaned, "merged_clean_data.txt")
> 
> ## Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.
> 
> uniqueSubjects = unique(S)[,1]
> numSubjects = length(unique(S)[,1])
> numActivities = length(activities[,1])
> numCols = dim(cleaned)[2]
> result = cleaned[1:(numSubjects*numActivities), ]
> 
> row = 1
> for (s in 1:numSubjects) {
+ for (a in 1:numActivities) {
+ result[row, 1] = uniqueSubjects[s]
+ result[row, 2] = activities[a, 2]
+ tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
+ result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
+ row = row+1
+       }
+ }
> write.table(result, "Tidy_data_set_with_the_averages.txt")
