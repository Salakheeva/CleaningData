library(reshape2)
# 1. Merges the training and the test sets to create one data set.
#training
train_x <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./UCI HAR Dataset/train/subject_train.txt")
train<-cbind(train_x,train_y,train_subject)
#test
test_x <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./UCI HAR Dataset/test/subject_test.txt")
test<-cbind(test_x,test_y,test_subject)
#merging
Data<-rbind(train,test)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
features <- read.table("./UCI HAR Dataset/features.txt")
ext<-grepl("[mM]ean|[Ss]td", features$V2)
Data<-Data[,c(ext,TRUE,TRUE)]
# 3. Uses descriptive activity names to name the activities in the data set
for (curRow in 1:nrow(labels))
{
  Data[[87]]<-gsub(labels[curRow,1],labels[curRow,2],Data[[87]])
}
# 4. Appropriately labels the data set with descriptive activity names.
features<-features[ext,]
features[,2] = gsub('[-()]', '', features[,2])
colnames(Data)<-tolower(c(features$V2, "Activity", "Subject"))
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
variable<-colnames(Data[,-c(87,88)])
Data_melt= melt(Data, id = c(87,88), measure.vars = variable)
Tidy_Data   = dcast(Data_melt, activity + subject ~ variable, mean)
write.table(Tidy_Data, file = "./tidy_data.txt",row.name=FALSE)
