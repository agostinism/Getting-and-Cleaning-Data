# Getting-and-Cleaning-Data
Repository for the Getting and Cleaning Course
The purpose of this project is to demonstrate your ability to collect, work with, 
and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

In ths repository you will find the files requested for the analyis on the Human Activity Recognition Using Smartphones Dataset experiment. Information about the experiment and the data used for this project can be found at 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The analysis performed consisted in tidying up the data, naming variables and setting categories to make it more redeable and then we proceeded to analysis the mean and standard deviation of the measurements in the data. he files that were used for this analysis are listed as follows:

tidi_data.txt:  tidy data set generated.
CodeBook.md:    file with a description of the tidy dataset
README.md:      This current file
run_analysis.R:	Current R script that generates the analysis.

The run_analysis.R script when run, goes through all the process to create the tidi_data.txt and provides answers to the following inquiries,

- Merges the training and the test sets to create one data set:<br>
  The program <b>run_analysis.R</b> the data frame <i>statistics</i> contains both the test and train data.
- Extracts only the measurements on the mean and standard deviation for each measurement:<br>
  The variable <i>mean_variable</i> obtains the mean and <i>std_variable</i> obtains the standard deviation from the statistics
  dataset.
- Uses descriptive activity names to name the activities in the data set:<br>
  The final tidi_data.txt contains the variables <i>task_performed_number</i> and <i>task_performed</i> displaying the activities and activity number performed in the experiment. 
- Appropriately labels the data set with descriptive variable names:<br>
  Variable names such as <i>subject</i> (1-30), <i>type</i> (<i>train</i>,<i>test</i>) and the previouly mentioned <i>task_performed</i>(<i>walking</i>, <i>sitting</i>,<i>standing</i>,<i>laying</i>,<i>walking downstairs</i>,<i>walking upstairs</i>) are part of the tidy dataset generated by the script.
- Create a second, independent tidy data set from the merged data set, with the average of each variable for each activity and each  subject:<br>
  The second independent tidy dataset is presented as solution for the analysis in the data frame <i>tidi_data_means</i>. This frame corresponds to the final file <b>tidi_data.txt</b> presented as part of the solution for the assignment.
  
  


