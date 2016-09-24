# run_analysis.R creates a single, tidy dataset from the two found 
# here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# if activity labels, subject files and X and y files are placed in working directory

#load dplyr
install.packages("dplyr")
library(dplyr)

# load test and training sets
test_set <- read.table("X_test.txt")
train_set <- read.table("X_train.txt")

# create variable for identifying test and training data
test_set <- mutate(test_set, group= "test")
train_set <- mutate(train_set, group = "train")

#merge data sets
merged_data <- rbind(test_set, train_set)
cnum <- ncol(merged_data)
merged_data <- merged_data[,c(cnum,1:(cnum-1))]

#load column names and add to table
variables <- read.table("features.txt", stringsAsFactors = FALSE)
variables <- variables[,2]
variables <- append("group",variables)
colnames(merged_data) <- variables
#choose mean and std data
descriptive_data <- merged_data[,grep("mean|std|group",colnames(merged_data))]

# add subjects
train_subs <- read.table("subject_train.txt")
test_subs <- read.table("subject_test.txt")
subjects <- rbind(train_subs, test_subs)
descriptive_data <- cbind(subjects, descriptive_data)
names(descriptive_data)[1] <- "subject"

#load activity descriptors, combine and add to desc_data
train_activity <- read.table("y_train.txt")
test_activity <- read.table("y_test.txt")
activity <- rbind(train_activity,test_activity)
activity[activity=="1"] <- "WALKING"
activity[activity=="2"] <- "WALKING_UPSTAIRS"
activity[activity=="3"] <- "WALKING_DOWNSTAIRS"
activity[activity=="4"] <- "SITTING"
activity[activity=="5"] <- "STANDING"
activity[activity=="6"] <- "LAYING"

descriptive_data <- cbind(activity, descriptive_data)
names(descriptive_data)[1] <- "activity"


#produce grouped statistics
grouped <-group_by(descriptive_data,activity, subject)
tidy_means <- summarise_each(grouped, funs(mean),4:ncol(grouped))

# create activity means
Activities <- group_by(descriptive_data, activity)
mean_Activities <- summarise_each(Activities, funs(mean),4:ncol(Activities))
mean_Activities <- mutate(mean_Activities, subject = "ALL")
mean_Activities <- mean_Activities[,c(1,81,2:80)]



# save files
write.csv(descriptive_data, file = "fulldatamean&std.csv")
write.csv(tidy_means, file = "tidy_means.csv")
write.csv(mean_Activities, file = "activity_means.csv")
write.table(tidy_means, file = "tidy_means.txt", row.name = FALSE)


