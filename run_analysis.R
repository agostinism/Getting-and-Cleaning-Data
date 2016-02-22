#install.packages("sqldf")
install.packages("reshape")
#library("sqldf")
setwd("C:\\Getting and Cleaning Data\\data\\UCI HAR Dataset")
activityNames<-read.table("activity_labels.txt")
activityTest<-read.table("test\\y_test.txt")
activityTrain<-read.table("train\\y_train.txt")
#Activity Names------------------------------------------------
df1=data.frame(activityNames)
names(df1)[2]<-"task_performed"
df2=data.frame(activityTest)
df3=data.frame(activityTrain)
task_order_test<-as.numeric(rownames(df2))
rownames(df2)<-NULL
data<-cbind(task_order_test,df2)
df2<-data
df4 <-sqldf("select df2.task_order_test,df1.V1,df1.task_performed from df1,df2 using(V1)")
task_order<-as.numeric(rownames(df3))
rownames(df3)<-NULL
dataTest<-cbind(task_order,df3)
df3<-dataTest
df5 <-sqldf("select df3.task_order,df1.V1,df1.task_performed from df3,df1 using(V1)")
#Activity setting-----------------------
activities_test<-subset(df4,task_order_test==task_order_test,select=task_order_test:task_performed)
activities_train<-subset(df5,task_order==task_order,select=task_order:task_performed)

all_activities_test<-activities_test[order(activities_test["task_order_test"]),]
all_activities_train<-activities_train[order(activities_train["task_order"]),]

names(all_activities_test)[2]<-"task_performed_number"
names(all_activities_train)[2]<-"task_performed_number"

all_activities_test$task_order_test<-NULL
all_activities_train$task_order<-NULL

columnLabels<-read.table("features.txt")
snames<-subset(columnLabels,select=V2)

subject_Test<-read.table("test\\subject_test.txt")
subject_Train<-read.table("train\\subject_train.txt")

df6<-data.frame(subject_Test)
df7<-data.frame(subject_Train)

names(df6)[1]<-"subject"
names(df7)[1]<-"subject"

Tests<-read.table("test\\X_test.txt")
Train<-read.table("train\\X_train.txt")

df8=data.frame(Tests)
colnames(df8)<-t(snames)
statistics_test<-cbind(df6,all_activities_test,df8)
statistics_test$type<-"test"

df9=data.frame(Train)
colnames(df9)<-t(snames)
statistics_train<-cbind(df7,all_activities_train,df9)
statistics_train$type<-"train"

# 1. Merging the training and test datasets

statistics<-rbind(statistics_test,statistics_train)

# 2. Extract Measurement of the mean and standard deviation for each measurements

mean_variable<-statistics[grep("-mean()",names(statistics),value=TRUE,fixed=TRUE)]
std_variable<-statistics[grep("-std()",names(statistics),value=TRUE,fixed=TRUE)]

# 3. Datasets have descriptive names.
# 4. Datasets have descriptive names and appropriate labels

library(reshape)

#5. Independent tidy dataset with the average for each variable for each activity and each subject
tidy_data<-melt(statistics,id.vars=c("subject","task_performed_number","task_performed","type"))

tidi_data_means<-cast(tidy_data,subject+task_performed_number+task_performed+type~variable,mean)

write.table(tidi_data_means,file="tidi_data.txt",row.name=FALSE)


