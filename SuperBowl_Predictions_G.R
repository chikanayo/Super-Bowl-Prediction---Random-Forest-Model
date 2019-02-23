getwd()
setwd("C:\\Users\\user\\xx\\xx")
getwd()

#Selecting the data set

nfldata <- read.csv(file.choose(), header=TRUE)

#Dropping Team and Opponent Score columns from data
library(dplyr)

nfldata2 <- select(nfldata, -c(Team_Score, Opponent_Score))

#Checking out the data

head(nfldata2)
str(nfldata2)

#Banana Split 80:20 - setting the training and validation split to 80:20

#setting seed so results can be replicated

set.seed(100)

train <- sample(nrow(nfldata), 0.8*nrow(nfldata), replace = FALSE)

tset <- nfldata2[train,]
vset <- nfldata2[-train,]

#Quick descriptive statistics to make sure nothing looks crazy

summary(tset)
summary(vset)

#Time To Grow Some Trees

install.packages('randomForest')
library(randomForest)

#First random forest pass

willows <- randomForest( OUTCOME~ ., data = tset, importance=TRUE)
willows

#Second pass with some paramter specifications

willows2 <- randomForest( OUTCOME~ ., data = tset, ntree=700, mtry=7, importance=TRUE)
willows2

#Predicting using training set and then checking classification accuracy

trainpredict <- predict(willows2, tset, type = 'class')
table(trainpredict, tset$OUTCOME)

#Doing the same thing on the validation set

validpredict <- predict(willows2, vset, type = 'class')
mean(validpredict == vset$OUTCOME)
table(validpredict, vset$OUTCOME)

#creating loop to test different node entries

accuracy=c()
i=5
for (i in 3:8) {
  hope <- randomForest(OUTCOME ~ ., data = tset, ntree = 1000, mtry = i, importance = TRUE)
  pvalid <- predict(hope, vset, type = "class")
  accuracy[i-2] = mean(pvalid == vset$OUTCOME)
}
accuracy

#Looks like 7 gives us the highest accuracy. Let's use that to train and validate our model.

weepingwillow <- randomForest( OUTCOME~ ., data = tset, ntree=1000, mtry=7, importance=TRUE)
weepingwillow

trainpred <- predict(weepingwillow, tset, type = "class")
table(trainpred, tset$OUTCOME)

#Touche but suspiciously good, lets see the validation set

validpred <- predict(weepingwillow, vset, type = "class")
table(validpred, vset$OUTCOME)

importance(weepingwillow)
varImpPlot(weepingwillow)

#Importing test set

finaltest <- read.csv(file.choose())

#Matching levels on test set with the training set
levels(finaltest$OUTCOME) <- levels(tset$OUTCOME)
levels(finaltest$ï..Team) <- levels(tset$ï..Team)
levels(finaltest$Opponent) <- levels(tset$Opponent)

#Predicting assuming both teams have an average game

finalpredict <- predict(weepingwillow, finaltest, type ="class")
table (finalpredict, finaltest$OUTCOME)
finalpredict


