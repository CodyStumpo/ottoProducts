library(nnet)
set.seed(342)
train<-read.csv("train.csv")
test<-read.csv("test.csv")
trainTransform <- log10(train[,-c(1,ncol(train))] + 1)
trainTransform <- cbind(id=train[,1], trainTransform)
trainTransform <- cbind(trainTransform, target=train[,ncol(train)])

fit<-nnet(target ~ ., train[,-1], size = 3, rang = 0.1, decay = 5e-4, maxit = 500)
predicted<-as.data.frame(predict(fit,test[,-1],type="raw"))
predicted = format(predicted, digits=2,scientific=F)
id<-test[,1]
output<-cbind(id,predicted)
write.csv(output,"NNetSubmission.csv",row.names=FALSE)
#before transform scores 0.76 with size = 3, rang = 0.1, decay = 5e-4, maxit = 500

