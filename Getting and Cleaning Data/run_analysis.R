# run_analysis.R

#Loading libraries and all the files into data tables

library(dplyr)

train_x <- data.table :: fread("./UCI HAR Dataset/train/X_train.txt")
train_y <- data.table :: fread("./UCI HAR Dataset/train/y_train.txt")
subject_train <- data.table :: fread("./UCI HAR Dataset/train/subject_train.txt")

test_x <- data.table :: fread("./UCI HAR Dataset/test/X_test.txt")
test_y <- data.table :: fread("./UCI HAR Dataset/test/y_test.txt")
subject_test <- data.table :: fread("./UCI HAR Dataset/test/subject_test.txt")

activity_labels <- data.table :: fread(("./UCI HAR Dataset/activity_labels.txt"))

# Merging features sets and renaming variables

x_df <- rbind(train_x,test_x)
colnames(x_df) <-  (data.table :: fread("./UCI HAR Dataset/features.txt"))$V2

# Subsetting feature set variables for mean and standad deviation

list_names <- grep("mean|std", names(x_df), value=TRUE)
x_subset <- subset(x_df, select=c(list_names))

#Merging activity sets and renaming variables

y_df <- rbind(train_y,test_y) 
y_df <- left_join(y_df,activity_labels,by="V1") %>% rename(Activity_ID = V1, Activity_Label = V2)

#Merging subject sets and renaming variables

subject_df <- rbind(subject_train, subject_test) %>% rename(Subject=V1)

#Merging features, activities, and subject sets and tidying data

combined_df <- cbind(x_subset, y_df, subject_df)
combined_df <- combined_df[,c(82,80,81,1:79)]

#Reshaping and casting 'combined_df' to get aggregated mean values

combined_df <- reshape2 :: melt(data = combined_df, id = c("Subject", "Activity_Label"))
tidy_data <- reshape2 :: dcast(data = combined_df, Subject + Activity_Label ~ variable, fun.aggregate = mean)

#Writin tidy_data mto text file
data.table::fwrite(x = tidy_data, file = "tidy_data.txt", quote = FALSE, row.names=FALSE)