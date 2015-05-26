#look into ipred & bagging

#meta-classifier

#rForest 0.51
rfsub <- read.csv(file = "submissions/180RFsubmit.csv",header = TRUE)
id=rfsub[,1]

#boostedTrees 0.51
btsub <- read.csv(file = "submissions/BTsubmission.csv",header = TRUE)
#xgBoost 0.51
xgbsub <- read.csv(file = "submissions/xgbSubmission.csv",header = TRUE)
#obt8 score around 0.45
obt8sub <- read.csv(file = "submissions/OBT2submission.csv",header = TRUE)
#xgBoost 0.49?
xgbsubOPT <- read.csv(file = "submissions/xgbSubmission-opt.csv",header = TRUE)

#prob should avg these four boosted tree things and treat it as one model
#let's mix bt first, submit that - then mix with other 3 methods
btwts = 1/c(.51, .51, .45, .49)
btwts = btwts / sum(btwts)
btsub = round(btwts[1]*btsub + btwts[2]*xgbsub + btwts[3] * obt8sub +btwts[3] * xgbsubOPT,3)
btsub = cbind(id, btsub[,-1])
write.csv(btsub, file="submissions/btSubmission.csv", row.names=FALSE)
#that's .45808 162/1215

#nnet 0.55
nnsub <- read.csv(file = "submissions/nnet-opt.csv",header = TRUE)
#nnsub[,6] <- pmax(0,1-rowSums(nnsub[,-c(1,6)]))
#don't know what went wrong with that column

#svm 0.57
svmsub <- read.csv(file = "submissions/svmSub.csv",header = TRUE)

#h2o 0.44?
h2osub <-read.csv(file = "submissions/h2osubmission9.csv",header = TRUE)/9
#h2069 .4462
h2osub69 <-read.csv(file = "submissions/h2osubmission69.csv",header = TRUE)/69


wts = 1/c(.46, .51, .55, .57,.44)
wts = wts/sum(wts)

en2sub = round(wts[1] * btsub + wts[2] * rfsub + wts[3]* nnsub + wts[4]* svmsub + wts[5]* h2osub,3)
en2sub = cbind(id, en2sub[,-1])
write.csv(en2sub, file="submissions/ensembleMaySubmission.csv", row.names=FALSE)
#.47826 (without h2o) - this doesn't help


#let's just try bt + h2o straight average
top2sub = round((btsub+h2osub)/2,3)
top2sub = cbind(id, top2sub[,-1])
write.csv(top2sub, file="submissions/top2Submission.csv", row.names=FALSE)
#.434

#let's just try bt + h2o69 straight average
top2sub = round((btsub+h2osub69)/2,3)
top2sub = cbind(id, top2sub[,-1])
write.csv(top2sub, file="submissions/top2Submission1.csv", row.names=FALSE)
#.434


#how about just obt8 & h2069
toptopsub = round((obt8sub+h2osub69)/2,3)
toptopsub = cbind(id, toptopsub[,-1])
write.csv(top2sub, file="submissions/toptopSubmission.csv", row.names=FALSE)
#.434

#SWAG WTS wts=c(0.16734220 0.08650489 0.08650489 0.07395030 0.16734220 0.41835550)
#.46728, 155 out of 1043
#ensub = round(wts[1]*rfsub + wts[2]*btsub + wts[3]*xgbsub + wts[4]*nnsub + wts[5]*svmsub + wts[6] * obt8sub,3)
#ensub = cbind(id, ensub[,-1])
#write.csv(ensub, file="submissions/ensembleSWAGSubmission.csv", row.names=FALSE)


#rf, bt, xgb, nn
"Score = .50432, 199 of 857. top = .42"

#rf, bt, xgb, nn, svm, gl with SWAG WEIGHTS
".48, 203 out of 949"




#this is actually kind of like gradient boosted trees itself, which blends weak classifiers and actually each iteration works on weakest predictions

"
Other to dos
#try pca before svm
#also naive bayes & logistic regression or something
#if fast enough, do cross-validation to estimate score
#linear discriminant analysis?
"
