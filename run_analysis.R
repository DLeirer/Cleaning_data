## title: "Cleaning Data Week Course Project scripts"



## load train data
X_columns <- read.table("UCI_HAR_Dataset/features.txt")
X_train <- read.table("UCI_HAR_Dataset/train/X_train.txt",col.names=X_columns[,2])
activity_train <- read.table("UCI_HAR_Dataset/train/y_train.txt")
subject_train <- read.table("UCI_HAR_Dataset/train/subject_train.txt")

X_train$activity <- activity_train[,1]
X_train$subject <- subject_train[,1]


## load test data
X_columns <- read.table("UCI_HAR_Dataset/features.txt")
X_test <- read.table("UCI_HAR_Dataset/test/X_test.txt",col.names=X_columns[,2])
activity_test <- read.table("UCI_HAR_Dataset/test/y_test.txt")
subject_test <- read.table("UCI_HAR_Dataset/test/subject_test.txt")

dim(X_test)
dim(X_columns)
dim(activity_test)
dim(subject_test)
str(X_test)
str(X_columns)
str(activity_train)
str(subject_train)

X_test$activity <- activity_test[,1]
str(X_test[1:10,560:563])
X_test$subject <- subject_test[,1]


##Subset to mean and std columns
testing_data <- X_train
training_data <- X_test

full_data <- rbind(training_data, testing_data)


cols <- names(full_data)
cols
selectedmean <- grep("mean", cols)
selectedstd <- grep("std", cols)
selecteddrop <- grep("meanFreq", cols)
selecteddrop
rightcols<-sort(c(selectedmean,selectedstd))
rightcols <- rightcols [! rightcols %in% selecteddrop]
length(rightcols)
rightcols



data_m_s <- full_data[,c(rightcols,562:563)]


##create mean for each category
data_m_s$cat = paste(data_m_s$subject, data_m_s$activity, sep="_")
data_m_s$cat<-sub("\"", "", data_m_s$cat)

m<-as.data.frame(sapply(data_m_s, function(x) gsub("\"", "", x)))
data_m_s$cat<-m$cat
data_m_s$cat<-as.numeric(data_m_s$cat)
data2 <- data_m_s
data2$cat <- m$cat


data2$cat <- as.numeric(data2$cat)


id <- names(table(data2$cat))
id
data_f <- data.frame()

for(i in id){
    temp_d <- data2[data2$cat==i,] 
    data_g <-data.frame(colMeans(temp_d))
    data_f <- rbind(data_f, t(data_g))    
}



##clean data frame
table(data_f$subject)
data_f$activity <- replace(data_f$activity, data_f$activity==1, "WALKING")
data_f$activity <- replace(data_f$activity, data_f$activity==2, "WALKING_UPSTAIRS")
data_f$activity <- replace(data_f$activity, data_f$activity==3, "WALKING_DOWNSTAIRS")
data_f$activity <- replace(data_f$activity, data_f$activity==4, "SITTING")
data_f$activity <- replace(data_f$activity, data_f$activity==5, "STANDING")
data_f$activity <- replace(data_f$activity, data_f$activity==6, "LAYING")
data_f$cat <- NULL


##save file
write.table(data_f, file ="final_data.txt")


