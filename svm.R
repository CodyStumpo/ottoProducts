train <- read.csv("train.csv")
test <- read.csv("test.csv")
# install.packages( 'e1071' )
library( 'e1071' )

#let's make a training set 10% of the rows or this will take forever
randRows = sample(1:61878,size = 6000, replace=FALSE)
tinyTrain=train[randRows,-1]

#looks like gamma=.01, cost = 2 is pretty good, so don't run this anymore


obj6000 <- tune(svm, target~., data = tinyTrain, probability=TRUE, 
                ranges = list(gamma = c(.001, .005, .01, .1, .5, 1 , 2), cost = c(1,10,100,1000)),  
                tunecontrol = tune.control(sampling = "cross"), best.model=TRUE
)


#this takes only 35 minutes w 6k rows.  n^2 time, so 3500 minutes for whole thing? (3 days)
#Let's see what tune actually outputs.  Claims to include best.tune(). best.model only if requested?
#uses "classification error" as performance measure

#best model has 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  2 
# gamma:  0.01 
# 
# Number of Support Vectors:  3637
#best performance is only .25?  That's percent misclassified

save(obj, file="tunedsvm.rda")


resp600 <- predict( obj600$best.model, test ,probability = TRUE)
resp600Prob <- attr(resp600, "probabilities")
#OK, this works, mechanically.  Columns are out of order though, and it's a matrix instead of df
r600df <- data.frame(resp600Prob)
r600df <- r600df[,order(names(r600df))]
#and then add id back in
id=test$id
r600df <- cbind(id, r600df)

#only run this when you're really ready. Will take a while. (45 min fit  +15 min predict)
svmFit <- svm( target ~ ., data=train[,-1], probability=TRUE, gamma=.01, cost=2 )
save(svmFit, file="svmFit.rda")
svmPredict <- predict(svmFit, test, probability=TRUE)
svmPredProb <-attr(svmPredict, "probabilities")
svmPPdf <- data.frame(svmPredProb)
svmSub <- cbind(id, round(svmPPdf,3))
write.csv(svmSub, file="svmSub.csv", row.names=FALSE)
