## Project
## R script "run_analysis.R":
## 1. Merge the training and the test sets to create one data set.
## 2. Extract only the measurements on the mean and standard deviation for each measurement. 
## 3. Use descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
##    with the average of each variable for each activity and each subject.

# 1. Merge the training and the test sets to create one data set.
## 1a. For X data set
tmp1 <- read.table("train/X_train.txt");
tmp2 <- read.table("test/X_test.txt");
X_table <- rbind(tmp1, tmp2);

## 1b. For Y data set
tmp1 <- read.table("train/Y_train.txt");
tmp2 <- read.table("test/Y_test.txt");
Y_table <- rbind(tmp1, tmp2);

## 1c. For S data set
tmp1 <- read.table("train/subject_train.txt");
tmp2 <- read.table("test/subject_test.txt");
S_table <- rbind(tmp1, tmp2);
names(S_table) <- "subject";

# 2. Extracts onlY the measurements on the mean and standard deviation for each measurement.
## 2a. Read features data set.
features_table <- read.table("features.txt");
## 2b. Get indices for mean and std deviation.
indices_for_mean_std <- grep("-mean\\(\\)|-std\\(\\)", features_table[, 2]);
X_table <- X_table[, indices_for_mean_std];
## 3b. Name the variables of X_table
names(X_table) <- features_table[indices_for_mean_std, 2];
names(X_table) <- gsub("\\(|\\)", "", names(X_table));
names(X_table) <- tolower(names(X_table));

# 3. Use descriptive activitY_table to name the activities_table in the data set.
## 3a. Read activities data set.
activities_table <- read.table("activity_labels.txt");
activities_table[, 2] = gsub("_", "", tolower(as.character(activities_table[, 2])));
## 3b. Match up the activity number in Y_table to activity_table. Save it to Y_table.
Y_table[,1] = activities_table[Y_table[,1], 2];
## 3c. Name the Y_table variable to activity.
names(Y_table) <- "activity";

# 4. create first combined data set
combined_table <- cbind(S_table, Y_table, X_table);
write.table(combined_table, "combined_data.txt");

# 5. create a second, independent tidy data set with the average of each variable for each activity and each subject.

USubjects = unique(S)[,1];
numOfSubjects = length(unique(S)[,1]);
numOfactivities= length(activities_table[,1]);
numOfColscombined = dim(combined_table)[2];
result_table = combined_table[1:(numOfSubjects*numOfactivities), ];

row = 1;
for (s in 1:numOfSubjects) {
  for (a in 1:numOfactivities) {
    result[row, 1] = USubjects[s]
    result[row, 2] = activities_table[a, 2]
    tmp <- combined_table[combined_table$subject==s & combined_table$activity==activities_table[a, 2], ]
    result[row, 3:numOfColscombined] <- colMeans(tmp[, 3:numOfColscombined])
    row = row+1
  }
}
write.table(result, "data_set_averages.txt");
