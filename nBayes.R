library(e1071)
train<-read.csv("train.csv")
test<-read.csv("test.csv")
fit<-naiveBayes(target ~ ., train[,-1], laplace=3)
predicted<-as.data.frame(predict(fit,test[,-1],type="raw"))
predicted = format(predicted, digits=2,scientific=F)
id<-test[,1]
output<-cbind(id,predicted)
write.csv(output,"NBayesSubmission.csv",row.names=FALSE)

#Probabilities almost always close to 1.  Score wil be poor
