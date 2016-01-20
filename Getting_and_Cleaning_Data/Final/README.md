# This is the Final Project for the 
# Getting and Cleaning Data Course

## Here is the Project Summary:

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example [this article](http://www.insideactivitytracking.com/data-science-activity-tracking-and-the-battle-for-the-worlds-top-sports-brand/). Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## This is how it works:

* The raw data is extracted into the working directory with directory structure intact
* `run_analysis.R` is the script file and is located in the working diectory
    + it is commented with the following so you can follow in the code itself
1) First step is to read in all of the data into temporary tables. I'm using strip.write to make sure nothing funky sneaks in, and also check.names when I'm naming columns so they don't get deprecated.
2) Now that we've got all the data, I'm going to column bind both sets together, and then rowbind both sets into one uber-dataframe
3) To conserve memory, I'll remove all the temporary files I no longer need
4) Now we're going to extract all the mean and standard deviation columns by first doing a logical text search for "mean()" and "std()" and then using that to subset the uber dataframe into a new one.
5) Remove the uber frame since it's no longer needed and large
6) We'll go through the activity column and replace the indexes with the actual names of the activity
7) Remove more unneeded items
8) Rename all the variables to something nice and easy
9) Sort the data by Subject and Activity
10) Create the nice tidy data set that reduces to one mean value for each variable associated with each activity and each subject
