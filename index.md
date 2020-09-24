---
title: "assignment_8.4"
output:
  html_document:
    keep_md: true
  pdf_document: default
---

The objective of this analysis will be create a model capable of respond the
question *What is the quality of the exercise being realized*. To do so it will
utilize the
[Weight Lifting Exercises](https://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har)
dataset.

First, the caret library will be loaded and the data read. The *na.strings*
parameter is changed to account to digited values that doesn't represent a
real value. Also, the predicted class is transformed to a factor variable.


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
data = read.csv('pml-training.csv', na.strings = c('', '#DIV/0!', 'NA'))
data$classe = as.factor(data$classe)
```

The first 7 columns representing time indicators and the user performing the
activity are removed as they aren't useful to predict new values.
Then, columns with a great number of null values are removed, as they can't be
properly estimated and used to make the predictions.


```r
data = data[, 8:160]
used_variables = which(colMeans(!is.na(data)) > .2)  # at least 20% without NA
dataremovenull = data[, used_variables]
```

After doing that, we can see that all predictor have appropriate types and that
all null values were removed.


```r
str(dataremovenull)
```

```
## 'data.frame':	19622 obs. of  53 variables:
##  $ roll_belt           : num  1.41 1.41 1.42 1.48 1.48 1.45 1.42 1.42 1.43 1.45 ...
##  $ pitch_belt          : num  8.07 8.07 8.07 8.05 8.07 8.06 8.09 8.13 8.16 8.17 ...
##  $ yaw_belt            : num  -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 -94.4 ...
##  $ total_accel_belt    : int  3 3 3 3 3 3 3 3 3 3 ...
##  $ gyros_belt_x        : num  0 0.02 0 0.02 0.02 0.02 0.02 0.02 0.02 0.03 ...
##  $ gyros_belt_y        : num  0 0 0 0 0.02 0 0 0 0 0 ...
##  $ gyros_belt_z        : num  -0.02 -0.02 -0.02 -0.03 -0.02 -0.02 -0.02 -0.02 -0.02 0 ...
##  $ accel_belt_x        : int  -21 -22 -20 -22 -21 -21 -22 -22 -20 -21 ...
##  $ accel_belt_y        : int  4 4 5 3 2 4 3 4 2 4 ...
##  $ accel_belt_z        : int  22 22 23 21 24 21 21 21 24 22 ...
##  $ magnet_belt_x       : int  -3 -7 -2 -6 -6 0 -4 -2 1 -3 ...
##  $ magnet_belt_y       : int  599 608 600 604 600 603 599 603 602 609 ...
##  $ magnet_belt_z       : int  -313 -311 -305 -310 -302 -312 -311 -313 -312 -308 ...
##  $ roll_arm            : num  -128 -128 -128 -128 -128 -128 -128 -128 -128 -128 ...
##  $ pitch_arm           : num  22.5 22.5 22.5 22.1 22.1 22 21.9 21.8 21.7 21.6 ...
##  $ yaw_arm             : num  -161 -161 -161 -161 -161 -161 -161 -161 -161 -161 ...
##  $ total_accel_arm     : int  34 34 34 34 34 34 34 34 34 34 ...
##  $ gyros_arm_x         : num  0 0.02 0.02 0.02 0 0.02 0 0.02 0.02 0.02 ...
##  $ gyros_arm_y         : num  0 -0.02 -0.02 -0.03 -0.03 -0.03 -0.03 -0.02 -0.03 -0.03 ...
##  $ gyros_arm_z         : num  -0.02 -0.02 -0.02 0.02 0 0 0 0 -0.02 -0.02 ...
##  $ accel_arm_x         : int  -288 -290 -289 -289 -289 -289 -289 -289 -288 -288 ...
##  $ accel_arm_y         : int  109 110 110 111 111 111 111 111 109 110 ...
##  $ accel_arm_z         : int  -123 -125 -126 -123 -123 -122 -125 -124 -122 -124 ...
##  $ magnet_arm_x        : int  -368 -369 -368 -372 -374 -369 -373 -372 -369 -376 ...
##  $ magnet_arm_y        : int  337 337 344 344 337 342 336 338 341 334 ...
##  $ magnet_arm_z        : int  516 513 513 512 506 513 509 510 518 516 ...
##  $ roll_dumbbell       : num  13.1 13.1 12.9 13.4 13.4 ...
##  $ pitch_dumbbell      : num  -70.5 -70.6 -70.3 -70.4 -70.4 ...
##  $ yaw_dumbbell        : num  -84.9 -84.7 -85.1 -84.9 -84.9 ...
##  $ total_accel_dumbbell: int  37 37 37 37 37 37 37 37 37 37 ...
##  $ gyros_dumbbell_x    : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ gyros_dumbbell_y    : num  -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 -0.02 ...
##  $ gyros_dumbbell_z    : num  0 0 0 -0.02 0 0 0 0 0 0 ...
##  $ accel_dumbbell_x    : int  -234 -233 -232 -232 -233 -234 -232 -234 -232 -235 ...
##  $ accel_dumbbell_y    : int  47 47 46 48 48 48 47 46 47 48 ...
##  $ accel_dumbbell_z    : int  -271 -269 -270 -269 -270 -269 -270 -272 -269 -270 ...
##  $ magnet_dumbbell_x   : int  -559 -555 -561 -552 -554 -558 -551 -555 -549 -558 ...
##  $ magnet_dumbbell_y   : int  293 296 298 303 292 294 295 300 292 291 ...
##  $ magnet_dumbbell_z   : num  -65 -64 -63 -60 -68 -66 -70 -74 -65 -69 ...
##  $ roll_forearm        : num  28.4 28.3 28.3 28.1 28 27.9 27.9 27.8 27.7 27.7 ...
##  $ pitch_forearm       : num  -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.9 -63.8 -63.8 -63.8 ...
##  $ yaw_forearm         : num  -153 -153 -152 -152 -152 -152 -152 -152 -152 -152 ...
##  $ total_accel_forearm : int  36 36 36 36 36 36 36 36 36 36 ...
##  $ gyros_forearm_x     : num  0.03 0.02 0.03 0.02 0.02 0.02 0.02 0.02 0.03 0.02 ...
##  $ gyros_forearm_y     : num  0 0 -0.02 -0.02 0 -0.02 0 -0.02 0 0 ...
##  $ gyros_forearm_z     : num  -0.02 -0.02 0 0 -0.02 -0.03 -0.02 0 -0.02 -0.02 ...
##  $ accel_forearm_x     : int  192 192 196 189 189 193 195 193 193 190 ...
##  $ accel_forearm_y     : int  203 203 204 206 206 203 205 205 204 205 ...
##  $ accel_forearm_z     : int  -215 -216 -213 -214 -214 -215 -215 -213 -214 -215 ...
##  $ magnet_forearm_x    : int  -17 -18 -18 -16 -17 -9 -18 -9 -16 -22 ...
##  $ magnet_forearm_y    : num  654 661 658 658 655 660 659 660 653 656 ...
##  $ magnet_forearm_z    : num  476 473 469 469 473 478 470 474 476 473 ...
##  $ classe              : Factor w/ 5 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
```

```r
colSums(is.na(dataremovenull))
```

```
##            roll_belt           pitch_belt             yaw_belt 
##                    0                    0                    0 
##     total_accel_belt         gyros_belt_x         gyros_belt_y 
##                    0                    0                    0 
##         gyros_belt_z         accel_belt_x         accel_belt_y 
##                    0                    0                    0 
##         accel_belt_z        magnet_belt_x        magnet_belt_y 
##                    0                    0                    0 
##        magnet_belt_z             roll_arm            pitch_arm 
##                    0                    0                    0 
##              yaw_arm      total_accel_arm          gyros_arm_x 
##                    0                    0                    0 
##          gyros_arm_y          gyros_arm_z          accel_arm_x 
##                    0                    0                    0 
##          accel_arm_y          accel_arm_z         magnet_arm_x 
##                    0                    0                    0 
##         magnet_arm_y         magnet_arm_z        roll_dumbbell 
##                    0                    0                    0 
##       pitch_dumbbell         yaw_dumbbell total_accel_dumbbell 
##                    0                    0                    0 
##     gyros_dumbbell_x     gyros_dumbbell_y     gyros_dumbbell_z 
##                    0                    0                    0 
##     accel_dumbbell_x     accel_dumbbell_y     accel_dumbbell_z 
##                    0                    0                    0 
##    magnet_dumbbell_x    magnet_dumbbell_y    magnet_dumbbell_z 
##                    0                    0                    0 
##         roll_forearm        pitch_forearm          yaw_forearm 
##                    0                    0                    0 
##  total_accel_forearm      gyros_forearm_x      gyros_forearm_y 
##                    0                    0                    0 
##      gyros_forearm_z      accel_forearm_x      accel_forearm_y 
##                    0                    0                    0 
##      accel_forearm_z     magnet_forearm_x     magnet_forearm_y 
##                    0                    0                    0 
##     magnet_forearm_z               classe 
##                    0                    0
```

After preparing the data, we will separate the data in training and test sets
using 80% of the data to train the model.


```r
set.seed(21)
in_train = createDataPartition(dataremovenull$classe, p = .8, list = F)
train_data = dataremovenull[in_train, ]
test_data = dataremovenull[-in_train, ]
```

To make the prediction, the random forest model were chosen.


```r
set.seed(21)
model = train(classe ~ ., method = 'rf', data=train_data)
```

The model got a 100% accuracy on the training set and over 99% on the test set.
As such, the model got excellent results capable of making efficient predictions
of the quality of the exercise performed.


```r
confusionMatrix(train_data$classe, predict(model,train_data))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 4464    0    0    0    0
##          B    0 3038    0    0    0
##          C    0    0 2738    0    0
##          D    0    0    0 2573    0
##          E    0    0    0    0 2886
## 
## Overall Statistics
##                                      
##                Accuracy : 1          
##                  95% CI : (0.9998, 1)
##     No Information Rate : 0.2843     
##     P-Value [Acc > NIR] : < 2.2e-16  
##                                      
##                   Kappa : 1          
##                                      
##  Mcnemar's Test P-Value : NA         
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            1.0000   1.0000   1.0000   1.0000   1.0000
## Specificity            1.0000   1.0000   1.0000   1.0000   1.0000
## Pos Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Neg Pred Value         1.0000   1.0000   1.0000   1.0000   1.0000
## Prevalence             0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1935   0.1744   0.1639   0.1838
## Detection Prevalence   0.2843   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      1.0000   1.0000   1.0000   1.0000   1.0000
```


```r
confusionMatrix(test_data$classe, predict(model,test_data))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1113    2    0    0    1
##          B    3  755    1    0    0
##          C    0    3  680    1    0
##          D    0    0   11  630    2
##          E    0    0    0    0  721
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9939          
##                  95% CI : (0.9909, 0.9961)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9923          
##                                           
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9973   0.9934   0.9827   0.9984   0.9959
## Specificity            0.9989   0.9987   0.9988   0.9961   1.0000
## Pos Pred Value         0.9973   0.9947   0.9942   0.9798   1.0000
## Neg Pred Value         0.9989   0.9984   0.9963   0.9997   0.9991
## Prevalence             0.2845   0.1937   0.1764   0.1608   0.1846
## Detection Rate         0.2837   0.1925   0.1733   0.1606   0.1838
## Detection Prevalence   0.2845   0.1935   0.1744   0.1639   0.1838
## Balanced Accuracy      0.9981   0.9961   0.9907   0.9972   0.9979
```
