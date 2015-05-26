
#to guess at score, need to split out random 80% rows for training, 
#run ALL these models and predict on held back 20% sample that we know answers for
#if we did that, we could estimate mixing parameters better.
require(doMC)
registerDoMC(8)

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



#try some nnet should be around .76 with size=3
library(nnet)
#just guess
fit<-nnet(target ~ ., train[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500)


#see if you can't do better.  just allow maxit much bigger?  Mess with other stuff too.
#normalizing features (z-score) could make things go faster.   Seems wrong since features are (0,n]
#code target to 1-of-C dummy with softmax activation
require(caret)
library(doParallel) #windows multicore
cl <- makeCluster(8)
registerDoParallel(cl)
cfit <- train(target ~ ., tinyTrain, method='nnet', maxit=100, MaxNWts=16000,
              tuneGrid = expand.grid(.size=c(1,5,10,50,100),.decay=c(0,.01,0.001, .0005)),
              trControl=trainControl(method = "repeatedcv", number=10, repeats=3)
              
)
#this takes 6 hours x 8 cores on desktop

save(cfit, file="cfit.rda") #bestTune = size 50, decay .01

pred<-as.data.frame(predict(cfit,tinyTest,type="prob")) 
MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = pred)
#0.61 at least that's better than 0.76 maybe really do some scaling first. 
#Maybe take those parameters and try more iterations, also try messing with other stuff

# it  |decay| size| score
# -----------------------
# 500 | 5e-4| 3   | .76 (rang=0.1)
# 200 | .01 | 50  | .64
# 50  | .01 | 50  | .575
# 50  | .02 | 50  | .564
# 50  | .03 | 50  | .571
# 50  | .01 | 30  | .583  
# 30  | .01 | 50  | .593
# 100 | .02 | 50  | .618
# 50  | .02 | 75  | .557
# 50  | .02 | 100 | .552 ***
# 50  | .025 | 120 | .568
# 50  | .02 | 100 | .6 (rang=.003)
# 500 | .001| 150 | 4 (rang=0.1) way overfits. Close to 1 for everything

pfit<-nnet(target ~ ., tinyTrain, size = 100, decay = .02, maxit = 50, MaxNWts=16000)
ppred<-as.data.frame(predict(pfit,tinyTest,type="raw")) 
MultiClassLogLoss(actual = trainsub[-randRows,-1],predicted = ppred)

#try doing scale, nnet converges to everything class 1
#stt = as.data.frame(cbind(scale(tinyTrain[,-94]), tinyTrain$target))
#colnames(stt)[94] <- "target"
#pfit<-nnet(target ~ ., stt, size = 100, decay = .02, maxit = 50, MaxNWts=16000)
#stTest = as.data.frame(scale(tinyTest))
#ppred<-as.data.frame(predict(pfit,stTest,type="raw")) 


"Let's just fit the full set and put it in the ensemble for now"
test <- read.csv("test.csv")
ffit<-nnet(target ~ ., train[,-1], size = 100, decay = .02, maxit = 50, MaxNWts=16000)
fpred<-as.data.frame(predict(ffit,test,type="raw")) 
id=test$id
fpred <- cbind(id, round(fpred,3))
write.csv(fpred, file="nnet-opt.csv", row.names=FALSE)
