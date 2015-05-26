# clear environment workspace
rm(list=ls())
# load data
train <- read.csv("train.csv")
test <- read.csv("test.csv")
sample_sub <- read.csv("sampleSubmission.csv")
#"There are a total of 93 numerical features, which represent counts of different events."
 # remove id column so it doesn't get picked up by the random forest classifier
 #train2 <- train[,-1]
 #trainids <- train[,1]
 #rm(train)
# install randomForest package
install.packages('randomForest')
library(randomForest)
# set a unique seed number so you get the same results everytime you run the below model,
# the number does not matter
set.seed(12)

#see rfcv().  What to do with it?
#see tuneRF() - will get best mtry (uses sqrt(numPredictors) otherwise)

#my laptop in R can try about 10 trees a minute with mtry = 9

#I got a score of 0.56 with RF. Tune RF with ntree=180 and mtry=9

# create a random forest model using the target field as the response and all 93 features as inputs
fit <- randomForest(target ~ ., data=train[,-1], importance=TRUE, ntree=180, mtry=9)

# create a dotchart of variable/feature importance as measured by a Random Forest
varImpPlot(fit)
save(fit, file="RF180fit.rda")
# use the random forest model to create a prediction
pred <- predict(fit,test,type="prob")
pred <- format(pred, digits=2,scientific=F)
submit <- data.frame(id = test$id, pred)
write.csv(submit, file = "180RFsubmit.csv", row.names = FALSE)

#100 trees takes about 10 minutes on my laptop. Scores 0.59
#180 trees scores 0.51

