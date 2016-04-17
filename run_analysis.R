
## Get path to feature names
pathnamesTrain <- paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
pathTrainingData <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt") 

traindata <- read.table(pathTrainingData,header = F) ## Read the Xtrain File
namestrain1 <- read.table(pathnamesTrain,header = F) ## Read the features.txt File
namestrain_trimmed <- namestrain1[-c(1)] ## remove first meaningless column
namestrain_trimmed_tovector <- as.vector(namestrain_trimmed$V2) #turn data frame into vector of names
names(traindata) <- namestrain_trimmed_tovector ## assign names to columns of the training data frame

pathTestData <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt") 


testdata <- read.table(pathTestData,header = F) ## Read the Xtest File
names(testdata) <- namestrain_trimmed_tovector ## assign names to columns of the test data frame

#  merge datasets (preliminary)
totaldata <-rbind(testdata,traindata)

#get activity labels for the train set
pathTrainingLabels <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt") 
train_labels <- read.csv(pathTrainingLabels,header = F)
#get activity labels for the test set
pathTestLabels <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt") 
test_labels <- read.csv(pathTestLabels,header = F)

#  append label sets
labelstotal <- rbind(test_labels,train_labels)

labelstotal[labelstotal==1] <-"WALKING"
labelstotal[labelstotal==2] <-"WALKING_UPSTAIRS"
labelstotal[labelstotal==3] <-"WALKING_DOWNSTAIRS"
labelstotal[labelstotal==4] <-"SITTING"
labelstotal[labelstotal==5] <-"STANDING"
labelstotal[labelstotal==6] <-"LAYING"
labelstotalvector <- as.vector(labelstotal)
names(labelstotalvector) <-"activity"

#get subject number for the train set
pathTrainingSubjects <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt") 
train_subjects <- read.csv(pathTrainingSubjects,header = F)
#get subject number for the test set
pathTestSubjects <-paste0(getwd(),"/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt") 
test_subjects <- read.csv(pathTestSubjects,header = F)

#  append label sets
subjectstotal <- rbind(test_subjects,train_subjects)
subjectstotalvector <- as.vector(subjectstotal)
colnames(subjectstotalvector) <-"subject"

totaldatawithsubjects <- cbind(totaldata,subjectstotalvector)

totaldatawithlabelsandsubjects <- cbind(totaldatawithsubjects,labelstotalvector)

#get columns of means for measurements
namesdata <- names(totaldatawithlabelsandsubjects) #get colnames
meancol <- grep("-mean\\(",namesdata,value = T)

#get columns of stdev for measurements
stdevcol <- grep("[Ss]td\\(",namesdata,value = T)

#merge list vector of colums of means and stdevs
keepcol <- c(meancol,stdevcol)

#drop columns from dataset not in keepcol list
only_means_and_std_data <- totaldatawithlabelsandsubjects[keepcol]
means_std_data_with_Subjects <- cbind(only_means_and_std_data,subjectstotalvector)
means_std_data_with_labelsandsubjects <- cbind(means_std_data_with_Subjects,labelstotalvector)

##
## FINAL MERGE AS PER ITEM 4 OF THE ASSIGNMENT 
##
dataassign1 <-means_std_data_with_labelsandsubjects

# invoke the dplyr library (necessary for grouping)
library(dplyr)
#get list of means only
onlymeansforfinaldataset <- c(meancol,"activity","subject")
prefinaldatasetbeforegrouping <- dataassign1[onlymeansforfinaldataset]
#dropstdev cols from final dataset before grouping by subject and activity

###
### THIS IS THE TIDY DATASET FOR ITEM 5 IN THE ASSIGNMENT
### average of each variable for each activity and each subject

finaldataset <- prefinaldatasetbeforegrouping %>% group_by(subject,activity) %>% summarise_each(funs(mean))


