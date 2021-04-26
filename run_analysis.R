#Read test and train data sets
x_test <- read.delim("test/X_test.txt", header = FALSE, sep="")
subject_test <- read.delim("test/subject_test.txt", header = FALSE)
y_test <- read.delim("test/y_test.txt", header = FALSE) 
x_train <- read.delim("train/X_train.txt", header = FALSE, sep="")
subject_train <- read.delim("train/subject_train.txt", header = FALSE)
y_train <- read.delim("train/y_train.txt", header = FALSE)

#Merge the data sets together
X <- cbind(subject_train, y_train, x_train)
Y <- cbind(subject_test, y_test, x_test)
Merged_Data <- rbind(X, Y)

#Add in names and filter to only mean and std dev observations
features <- read.table("features.txt")
datanames <- c("person", "activity", as.character(features$V2))
mean_features <- grepl("person|activity|mean()|std()|meanFreq()", datanames)
Tidy_Data <- Merged_Data[, mean_features == TRUE]

#Add and rename the attributes
activity_labels <- read.table("activity_labels.txt")
Tidy_Data[, 2] <- activity_labels[Tidy_Data[, 2], 2]
x <- datanames[mean_features == TRUE]
x <- sub("tBody", "TimeBody", x)
x <- sub("tGravity", "TimeGravity", x)
x <- sub("fBody", "FreqBody", x)
x <- sub("BodyBody", "Body", x)
x <- sub("-mean\\(\\)-X", "X.mean", x)
x <- sub("-mean\\(\\)-Y", "Y.mean", x)
x <- sub("-mean\\(\\)-Z", "Z.mean", x)
x <- sub("-std\\(\\)-X", "X.sd", x)
x <- sub("-std\\(\\)-Y", "Y.sd", x)
x <- sub("-std\\(\\)-Z", "Z.sd", x)
x <- sub("-meanFreq\\(\\)-X", "X.meanFreq", x)
x <- sub("-meanFreq\\(\\)-Y", "Y.meanFreq", x)
x <- sub("-meanFreq\\(\\)-Z", "Z.meanFreq", x)
x <- sub("-mean\\(\\)", ".mean", x)
x <- sub("-std\\(\\)", ".sd", x)
x <- sub("-meanFreq\\(\\)", ".meanFreq", x)
names(Tidy_Data) <- x

#Use dplyr package to summarize the tidy data
ans <- Tidy_Data %>%
  arrange(person, activity) %>%
  group_by(person, activity) %>%
  summarise_all(mean)
ans <- as.data.frame(ans)

#Read out to text file
write.table(ans, "Tidy_Samsung_Data.txt", row.name=FALSE, sep = " ")
