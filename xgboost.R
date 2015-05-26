require(xgboost)
require(methods)

train = read.csv('train.csv',header=TRUE,stringsAsFactors = F)
test = read.csv('test.csv',header=TRUE,stringsAsFactors = F)
train = train[,-1]
test = test[,-1]

y = train[,ncol(train)]
y = gsub('Class_','',y)
y = as.integer(y)-1 #xgboost take features in [0,numOfClass)

x = rbind(train[,-ncol(train)],test)
x = as.matrix(x)
x = matrix(as.numeric(x),nrow(x),ncol(x))
trind = 1:length(y)
teind = (nrow(train)+1):nrow(x)

# Set necessary parameter
param <- list("objective" = "multi:softprob",
              "eval_metric" = "mlogloss",
              "num_class" = 9,
              "nthread" = 2, 
              "max_depth"=5, "eta"=0.3, "min_child_weight"=0.3, "gamma"=0.3,
              "colsample"=.6, "subsample"=.8)

#"max_depth"=6, "eta"=0.3, "min_child_weight"=1, "gamma"=1, "colsample"=1, "subsample"=1)
# Run Cross Validation
cv.nround = 200
bst.cv = xgb.cv(param=param, data = x[trind,], label = y,
                nfold = 3, nrounds=cv.nround)

# Train the model
 
#around 200 iterations, test logloss flattens out.  train keeps going down of course.
#with max_depth = 6.  Flattens around 100 with depth = 8

# Make prediction
pred = predict(bst,x[teind,])
pred = matrix(pred,9,length(pred)/9)
pred = t(pred)

# Output submission
pred = format(pred, digits=2,scientific=F) # shrink the size of submission
pred = data.frame(1:nrow(pred),pred)
names(pred) = c('id', paste0('Class_',1:9))
write.csv(pred,file='xgbSubmission-opt.csv', quote=FALSE,row.names=FALSE)

#score .51 unopt
#score .49 opt?
