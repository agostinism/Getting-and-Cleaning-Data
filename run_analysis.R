#**********************************************************************
#* Project           : Getting and Cleaning Data
#*
#* Program name      : run_analysis.R
#*
#* Author            : SMA (Simon Mendoza-Agostini)
#*
#* Date created      : 29FEB2016
#*
#* Purpose           : Analysis and generation of tidy data on the 
#*                     Human Activity Recognition Using Smartphones Dataset experiment
#*
#* Revision History  :
#*
#* Date        Author      Ref    Revision (Date in DDMMMYYYY format) 
#* 02MAR2016   SMA          1     Comments and Heading for readability. 
#*
#**********************************************************************
#
#----------------------------------------------------------------------
#  Packages installed: sqldf (for sql related syntax),reshape (transposing)
#----------------------------------------------------------------------
install.packages("sqldf")
install.packages("reshape")
library("sqldf")

#----------------------------------------------------------------------
#  Setting working directory
#----------------------------------------------------------------------
setwd("C:\\Getting and Cleaning Data\\data\\UCI HAR Dataset")

#----------------------------------------------------------------------
#  Activity Names read from raw data 
#  Activity data read from both for training and test. 
#----------------------------------------------------------------------
                                           
activityNames<-read.table("activity_labels.txt")
activityTest<-read.table("test\\y_test.txt")
activityTrain<-read.table("train\\y_train.txt")

#----------------------------------------------------------------------
#  Set new variable "task_performed"
#----------------------------------------------------------------------
dfActivityNames=data.frame(activityNames)
names(dfActivityNames)[2]<-"task_performed"

#----------------------------------------------------------------------
#  Activity Test block
#  set to data frame
#  set order of tasks and generate data frame with all test data
#  with appropriate variable set 
#----------------------------------------------------------------------

dfActivityTest=data.frame(activityTest)

task_order_test<-as.numeric(rownames(dfActivityTest))
rownames(dfActivityTest)<-NULL
data<-cbind(task_order_test,dfActivityTest)
dfActivityTest<-data
preActivityTest <-sqldf("select dfActivityTest.task_order_test,dfActivityNames.V1,dfActivityNames.task_performed from dfActivityNames,dfActivityTest using(V1)")

#----------------------------------------------------------------------
#  Activity Train block
#  set to data frame
#  set order of tasks and generate data frame with all test data
#  with appropriate variable set 
#----------------------------------------------------------------------

dfActivityTrain=data.frame(activityTrain)

task_order<-as.numeric(rownames(dfActivityTrain))
rownames(dfActivityTrain)<-NULL
dataTest<-cbind(task_order,dfActivityTrain)
dfActivityTrain<-dataTest
preActivityTrain <-sqldf("select dfActivityTrain.task_order,dfActivityNames.V1,dfActivityNames.task_performed from dfActivityTrain,dfActivityNames using(V1)")

#----------------------------------------------------------------------
#  Activity Train and Test 
#  set to contain needed data with appropriate variable naming 
#  "task_performed" and "task_performed_number" 
#----------------------------------------------------------------------

activities_test<-subset(preActivityTest,task_order_test==task_order_test,select=task_order_test:task_performed)
activities_train<-subset(preActivityTrain,task_order==task_order,select=task_order:task_performed)

all_activities_test<-activities_test[order(activities_test["task_order_test"]),]
all_activities_train<-activities_train[order(activities_train["task_order"]),]

names(all_activities_test)[2]<-"task_performed_number"
names(all_activities_train)[2]<-"task_performed_number"

all_activities_test$task_order_test<-NULL
all_activities_train$task_order<-NULL

#----------------------------------------------------------------------
#  Column label processing for both Train and Test 
#----------------------------------------------------------------------

columnLabels<-read.table("features.txt")
snames<-subset(columnLabels,select=V2)

#----------------------------------------------------------------------
#  Subject processing for Training and Test
#  assign "subject" variable name to dataset
#----------------------------------------------------------------------
subject_Test<-read.table("test\\subject_test.txt")
subject_Train<-read.table("train\\subject_train.txt")

dfSubjectTest<-data.frame(subject_Test)
dfSubjectTrain<-data.frame(subject_Train)

names(dfSubjectTest)[1]<-"subject"
names(dfSubjectTrain)[1]<-"subject"

#----------------------------------------------------------------------
#  processing Data for Training and Test  
#  assiging column names
#----------------------------------------------------------------------
Tests<-read.table("test\\X_test.txt")
Train<-read.table("train\\X_train.txt")

dfTest=data.frame(Tests)
colnames(dfTest)<-t(snames)
statistics_test<-cbind(dfSubjectTest,all_activities_test,dfTest)
statistics_test$type<-"test"

dfTrain=data.frame(Train)
colnames(dfTrain)<-t(snames)
statistics_train<-cbind(dfSubjectTrain,all_activities_train,dfTrain)
statistics_train$type<-"train"

#----------------------------------------------------------------------
# 1. Merging the training and test datasets
#----------------------------------------------------------------------
statistics<-rbind(statistics_test,statistics_train)

#----------------------------------------------------------------------
# 2. Extract Measurement of the mean and standard deviation for each measurements
#----------------------------------------------------------------------

mean_variable<-statistics[grep("-mean()",names(statistics),value=TRUE,fixed=TRUE)]
std_variable<-statistics[grep("-std()",names(statistics),value=TRUE,fixed=TRUE)]

#----------------------------------------------------------------------
# 3. Datasets have descriptive names.
# 4. Datasets have descriptive names and appropriate labels
#----------------------------------------------------------------------
library(reshape)

#----------------------------------------------------------------------
#5. Independent tidy dataset with the average for each variable for each activity and each subject
#----------------------------------------------------------------------

tidy_data<-melt(statistics,id.vars=c("subject","task_performed_number","task_performed","type"))
tidi_data_means<-cast(tidy_data,subject+task_performed_number+task_performed+type~variable,mean)

write.table(tidi_data_means,file="tidi_data.txt",row.name=FALSE)


