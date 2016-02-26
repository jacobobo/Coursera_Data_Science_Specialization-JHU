# Exercise Performance Quality
JBrand  

## Introduction
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: [http://groupware.les.inf.puc-rio.br/har](http://groupware.les.inf.puc-rio.br/har) (see the section on the Weight Lifting Exercise Dataset).

The data itself is available at the following two links:  
1. Training: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv)  
2. Testing: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv)

## Importing, Cleaning and Splitting Data

The first step is to import the .csv files listed above.

```r
training <- read.csv("pml-training.csv", na.strings = c("NA", "#DIV/0!", ""))
testing <- read.csv("pml-testing.csv", na.strings = c("NA", "#DIV/0!", ""))
```
Now that the files are loaded, we need to do some cleaning. 160 variables is a lot to process, and my computer doesn't have that kind of HP. Right off the bat, the first 7 columns are identification only, so they can go, and many columns are empty, so they will be removed as well.

```r
## Remove the first 7 columns
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]

## Remove the NA columns
training <- training[, colSums(is.na(training)) == 0]
testing <- testing[, colSums(is.na(testing)) == 0]
```
Now that we've reduced the variable count from 160 to 53, we have something more manageable to work with. In this case, since we have a very large training set, we have the ability to break it down into a training and validation set. So we'll do that now.

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
set.seed(55722)
inTrain <- createDataPartition(training$classe, p = 0.7, list = FALSE)
trainSet <- training[inTrain,]
validSet <- training[-inTrain,]
```

## Analysis
Based on the courses, it's been stated that the best prediction algorithms will be based on random forests, boosting, and some combination through ensembling.  Since there is a lot of data available, I will start with random forests on its own and see if the accuracy/out-of-sample error hold up.

```r
modFit <- train(classe~.,data = trainSet, method = "rf")
```

```
## Loading required package: randomForest
```

```
## randomForest 4.6-12
```

```
## Type rfNews() to see new features/changes/bug fixes.
```

```
## 
## Attaching package: 'randomForest'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     margin
```
Now that we've trained a model, we can check it out on our validation set that we created.

```r
valPredict <- predict(modFit, validSet)
confusionMatrix(valPredict,validSet$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1670    4    0    0    0
##          B    4 1134    2    0    0
##          C    0    1 1023    8    0
##          D    0    0    1  955   12
##          E    0    0    0    1 1070
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9944          
##                  95% CI : (0.9921, 0.9961)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9929          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9976   0.9956   0.9971   0.9907   0.9889
## Specificity            0.9991   0.9987   0.9981   0.9974   0.9998
## Pos Pred Value         0.9976   0.9947   0.9913   0.9866   0.9991
## Neg Pred Value         0.9991   0.9989   0.9994   0.9982   0.9975
## Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
## Detection Rate         0.2838   0.1927   0.1738   0.1623   0.1818
## Detection Prevalence   0.2845   0.1937   0.1754   0.1645   0.1820
## Balanced Accuracy      0.9983   0.9972   0.9976   0.9940   0.9944
```
As shown, there is incredible accuracy of __99.4%__ which means an out-of-sample error of __0.6%__.  That's certainly good enough for this analysis. 

## Conclusion
Now that we've created a model based on the training set and validated it using the validation set, we can finally predict the test set provided.

```r
testPredict <- predict(modFit, testing)
testPredict
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```
So, using the random forests model created, the test cases were evaluated. Based on the validation that we did, we can be very confident that these predicted values are accurate.
