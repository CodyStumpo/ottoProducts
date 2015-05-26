
#to guess at score, need to split out random 80% rows for training, 
#run ALL these models and predict on held back 20% sample that we know answers for
#if we did that, we could estimate mixing parameters better.
library(doMC)
registerDoMC(2)

split=.8
train <- read.csv("train.csv")
nr = nrow(train)
keep = floor(nr*split)
check = nr-keep
randRows = sample(1:nr, size =keep,replace=FALSE)
tinyTrain=train[randRows,-1]
tinyTest = train[-randRows, -c(1,ncol(train))]

sample_sub <- read.csv("sampleSubmission.csv")


trainsub=sample_sub[1:nrow(train),]
rm(sample_sub)
trainsub[,-1]<-0
library("stringr")

#this takes 30 seconds, save?
for(i in 1:nrow(train)) {
  cls = as.numeric(str_sub(train[i,95],-1))
  trainsub[i,cls+1]=1
}

meansub=trainsub[1:check,]
cm=round(colMeans(trainsub[,-1]),3)
for(i in 1:9) meansub[,i+1]=cm[i]

MultiClassLogLoss <- function(actual, predicted, eps=1e-15) {
  predicted[predicted < eps] <- eps;
  predicted[predicted > 1 - eps] <- 1 - eps;
  -1/nrow(actual)*(sum(actual*log(predicted)))
}


MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = meansub[,-1])
#predict mean for each column -> 1.95

unisub = meansub
unisub[,-1]<-1/9
MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = unisub[,-1])
#Uniform 1/9th probability would score 2.2


#try some nnet should be around .76
library(nnet)
fit<-nnet(target ~ ., tinyTrain, size = 150, rang = 0.1, decay = .001, maxit = 500, MaxNWts=16000)
pred<-as.data.frame(predict(fit,tinyTest,type="raw")) 
MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = pred)

#see if you can't do better.  just allow maxit much bigger?  Mess with other stuff too.
#normalizing features (z-score) could make things go faster.   Seems wrong since features are (0,n]
#code target to 1-of-C dummy with softmax activation
require(caret)

cfit <- train(target ~ ., tinyTrain, method='nnet', maxit=100,
              tuneGrid = expand.grid(.size=c(1,5,10),.decay=c(0,.01,0.001))
              )
#trControl=trainControl(method = "repeatedcv", number=10, repeats=3)

#log transform for nn
tinyTrainTransform <- log10(tinyTrain[,-ncol(tinyTrain)] + 1)
tinyTrainTransform <- cbind(tinyTrainTransform, target=tinyTrain[,ncol(tinyTrain)])

tinyTestTransform <- log10(tinyTest + 1)

fit<-nnet(target ~ ., tinyTrainTransform, size = 150, rang = 0.1, decay = .001, maxit = 50, MaxNWts=16000)
pred<-as.data.frame(predict(fit,tinyTestTransform,type="raw")) 
MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = pred)
#0.57, tune parameters on transform now - see if can get better than 0.55

#just guess
fit<-nnet(target ~ ., train[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500)

#so estimate xgboost-opt
