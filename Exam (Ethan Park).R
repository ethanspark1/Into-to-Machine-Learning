this.dir=getwd()
setwd(this.dir)

rm(list=ls())

set.seed(1)
ExamData=read.table('examdata.csv',sep=',',header=TRUE)
x=model.matrix(y~.,ExamData)[,-1]
p=ncol(x)
y=ExamData$y
train=seq(1,200) 
#The first half is a training sample. The second half is a test sample


#Linear model
LM.fit=lm(y~.,data=ExamData,subset=train)
LM.pred=predict(LM.fit,ExamData[-train,])
LM.MSE=mean((y[-train]-LM.pred)^2)
print(c('Linear model',LM.MSE))

####################### Your methods start from here ###############################
#Best Subset Selection
library(leaps)
regfit.full=regsubsets(y~.,data=ExamData,subset=train, nvmax = 20)
summary(regfit.full)
regsum=summary(regfit.full)
plot(regsum$cp, xlab = "Number of Variables", ylab = "CP")
plot(regsum$bic, xlab = "Number of Variables", ylab = "BIC")
which.min(regsum$cp)
which.min(regsum$bic)
#Testing The results of the lowest BIC 
reg5=lm(y~x01+x02+x03+x04+x05, data = ExamData, subset = train)
lm.reg5=predict(reg5, ExamData[-train,])
reg5.MSE=mean((y[-train]-lm.reg5)^2)
print(c('Linear model',reg5.MSE))
#Testing The results of the lowest Cp
reg7=lm(y~x01+x02+x03+x04+x05+x09+x15, data = ExamData, subset = train)
lm.reg7=predict(reg7, ExamData[-train,])
reg7.MSE=mean((y[-train]-lm.reg7)^2)
print(c('Linear model',reg7.MSE))
#The model suggested by the lowest BIC has a lower Test MSE suggesting that it is a superior model
library(gbm)
library(glmnet)
#ridge regression
set.seed(2)
grid<-10^seq(4,-2, length=10000)
train.mat<-model.matrix(y~., data = ExamData[train,])
test.mat<-model.matrix(y~., data = ExamData[-train,])
traindata<-ExamData[train,]
testdata<-ExamData[-train,]
fit.ridge<-glmnet(train.mat, traindata$y, alpha = 0, lambda = grid)
cv.ridge<-cv.glmnet(train.mat,traindata$y,alpha=0, lambda = grid)
bestlam.ridge<-cv.ridge$lambda.min
bestlam.ridge
pred.ridge<- predict(fit.ridge, s=bestlam.ridge, newx = test.mat)
mean((pred.ridge-testdata$y)^2)
#The ridge regression did slightly worse than the best subset method
#Lasso regression
fit.lasso<-glmnet(train.mat, traindata$y, alpha = 1, lambda = grid)
cv.lasso<-cv.glmnet(train.mat,traindata$y,alpha=1, lambda = grid)
bestlam.lasso<-cv.lasso$lambda.min
bestlam.lasso
pred.lasso<- predict(fit.lasso, s=bestlam.lasso, newx = test.mat)
mean((pred.lasso-testdata$y)^2)
lasso.coef=predict(fit.lasso, s=bestlam.lasso, type = 'coefficients')
lasso.coef
#PCR
library(pls)
fit.pcr<-pcr(y~., data=traindata, scale=T, validation='CV')
validationplot(fit.pcr, val.type = 'MSEP')
pred.pcr<-predict(fit.pcr, testdata, ncomp = 20)
mean((pred.pcr-testdata$y)^2)
#PLS
fit.pls<-plsr(y~., data=traindata, scale=T, validation="CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls<-predict(fit.pls, testdata, ncomp=20)
mean((pred.pls-testdata$y)^2)
#GAM mfrow should be changed depending on the model
#the first GAM model is the x01-x05 variable model
install.packages('gam')
library(gam)
gammodel<- gam(y~s(x01, df=2)+ s(x02, df=2)+ s(x03, df=2)+ s(x04, df=2) + s(x05,df=2), data=traindata)
par(mfrow=c(1,1))
plot(gammodel,se=T, col='red')
preds<-predict(gammodel, testdata)
preds
gamerr<-mean((testdata$y-preds)^2)
gamerr
gamerr1<-mean((preds-testdata$y)^2)
gamerr1
anova(gammodel)
summary(gammodel)
#GAM model with x15 and x20 variables
gammodel<- gam(y~s(x01, df=2)+ s(x02, df=2)+ s(x03, df=2)+ s(x04, df=2) + s(x05,df=2) + s(x15, df=2) +s(x20, df=2), data=traindata)
par(mfrow=c(1,1))
plot(gammodel,se=T, col='red')
preds<-predict(gammodel, testdata)
preds
gamerr<-mean((testdata$y-preds)^2)
gamerr
gamerr1<-mean((preds-testdata$y)^2)
gamerr1
summary(gammodel)
#GAM model with x01-x05 and x09 
gammodel<- gam(y~s(x01, df=2)+ s(x02, df=2)+ s(x03, df=2)+ s(x04, df=2) + s(x05,df=2)+s(x09, df=2), data=traindata)
par(mfrow=c(1,1))
plot(gammodel,se=T, col='red')
preds<-predict(gammodel, testdata)
preds
gamerr<-mean((testdata$y-preds)^2)
gamerr
gamerr1<-mean((preds-testdata$y)^2)
gamerr1
anova(gammodel)
summary(gammodel)
#GAM model with x01-x05 and x15
gammodel<- gam(y~s(x01, df=2)+ s(x02, df=2)+ s(x03, df=2)+ s(x04, df=2) + s(x05,df=2)+s(x15, df=2), data=traindata)
par(mfrow=c(1,1))
plot(gammodel,se=T, col='red')
preds<-predict(gammodel, testdata)
preds
gamerr<-mean((testdata$y-preds)^2)
gamerr
gamerr1<-mean((preds-testdata$y)^2)
gamerr1
anova(gammodel)
summary(gammodel)
#GAM model with x01-x05 and x20
gammodel<- gam(y~s(x01, df=2)+ s(x02, df=2)+ s(x03, df=2)+ s(x04, df=2) + s(x05,df=2) +s(x20, df=2), data=traindata)
par(mfrow=c(1,1))
plot(gammodel,se=T, col='red')
preds<-predict(gammodel, testdata)
preds
gamerr<-mean((testdata$y-preds)^2)
gamerr
gamerr1<-mean((preds-testdata$y)^2)
gamerr1
summary(gammodel)
#Trees training
set.seed(3)
lambda<-10^seq(-10,-0.2, by=0.1)
trainerr<-rep(NA, length(lambda))
for (i in 1:length(lambda)){
  boost.data<-gbm(y~., data = traindata, distribution = 'gaussian', n.trees = 1000, shrinkage = lambda[i])
  pred.train<-predict(boost.data, traindata, n.trees=1000)
  trainerr[i]<-mean((pred.train-traindata$y)^2)
}
plot(lambda, trainerr, type = 'b', xlab = 'Shrinkage values', ylab = 'train MSE')
#trees test
set.seed(3)
testerr<-rep(NA, length(lambda))
for (i in 1:length(lambda)){
  boost.data<-gbm(y~., data = traindata, distribution = 'gaussian', n.trees = 1000, shrinkage = lambda[i])
  yhat<-predict(boost.data, testdata, n.trees=1000)
  testerr[i]<-mean((yhat-testdata$y)^2)
}
plot(lambda, testerr, type = 'b', xlab = 'Shrinkage values', ylab = 'test MSE')
min(testerr)
lambda[which.min(testerr)]
boost.data<-gbm(y~., data = traindata, distribution = 'gaussian', n.trees = 1000, shrinkage = lambda[which.min(testerr)])
summary(boost.data)






