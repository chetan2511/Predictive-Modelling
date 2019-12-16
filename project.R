#loading libraries

library(car)
library(caret)
library(class)
library(devtools)
library(e1071)
library(ggord)
library(ggplot2)
library(Hmisc)
library(klaR)
library(MASS)
library(nnet)
library(plyr)
library(pROC)
library(psych)
library(scatterplot3d)
library(SDMTools)
library(dplyr)
library(ElemStatLearn)
library(rpart)
library(rpart.plot)
library(randomForest)
library(neuralnet)

#Import the data related cellphone
cell <- read.csv("Cellphone.csv")
str(cell)
#Check for missing values in each column in the data set
sapply(cell, function(x) sum(is.na(x)))
attach(cell)
View(cell)
hist(CustServCalls)
summary(cell)
mean(AccountWeeks)
hist(AccountWeeks)
hist(MonthlyCharge)
hist(OverageFee)

qplot(OverageFee,CustServCalls,data=cell,color=Churn)
qplot(OverageFee,AccountWeeks,data=cell,color=Churn)
qplot(OverageFee,AccountWeeks,data=cell,color=Churn==1)
qplot(AccountWeeks,CustServCalls,data=cell,color=Churn==1)
#from the above qplots plotted we can say that as the cust service calls 
#increases the cust. is likely to churn

cell.scatter<-subset(cell[,c(2:11)])
cell.scatter
cor(cell.scatter)

library(DataExplorer)
plot_correlation(cell)
#dataplan and data usage are related;monthly charge related with data plan and data usage

ggplot(cell,aes(x=Churn))+geom_histogram(binwidth = 1,fill=c('Blue','red'))+labs(title='Customer Churn',x='Churn',y='Freq')
#determines the ratio of customers from the dataset who had churned
qplot(DayCalls,CustServCalls,data=cell,geom=c('point','smooth'),colour=Churn==1)
#cust serv calls above 2 and day calls above 50 are more likely to churn

#churnrate
churnrate<-table(cell$Churn)/nrow(cell)
churnrate

#outliers
options(repr.plot.width=5,repr.plot.height=5)
boxplot(DataUsage)$out
boxplot(AccountWeeks)$out
boxplot(CustServCalls)$out
boxplot(DayCalls)$out
boxplot(DayMins)$out
boxplot(MonthlyCharge)$out
boxplot(OverageFee)$out
boxplot(RoamMins)$out


###cell$Churn1<-factor(cell$Churn)
#str(cell$Churn1)
#View(cell)

#cell.scaled=scale(cell[,1:11],center = T, scale = T)
#cell.scaled###

set.seed(300)
pd<-sample(2,nrow(cell),replace=TRUE, prob=c(0.65,0.35))

train<-cell[pd==1,]
val<-cell[pd==2,]

sum(cell$Churn)
sum(val$Churn)
sum(train$Churn)

train.reg<-train[,c(1:11)]
val.reg<-val[,c(1:11)]

#normalize<-function(x){
 # +return((x-min(x))/(max(x)-min(x)))}
#train.reg$norm.DataPlan<-normalize(train.reg$DataPlan)
#OLS.norm<-lm(Churn~norm.DataPlan, data=train.reg)
#summary(OLS.norm)
View(train.reg)
str(train.reg)
OLS.full<-lm(MonthlyCharge~Churn+AccountWeeks+ContractRenewal+DataUsage+CustServCalls+DayMins+DayCalls+OverageFee+RoamMins+DataPlan, data=train.reg)
summary(OLS.full)
vif(OLS.full)

#removing data usage from the dataset as its showing multicollinearity with dataplan and monthlycharge using corrplot
#final_data=MonthlyCharge~Churn+AccountWeeks+ContractRenewal+CustServCalls+DayMins+DayCalls+OverageFee+RoamMins+DataPlan

OLS.full1<-lm(MonthlyCharge~Churn+AccountWeeks+ContractRenewal+CustServCalls+DayMins+DayCalls+OverageFee+RoamMins+DataPlan, data=train.reg)
summary(OLS.full1)
vif(OLS.full1)

pred.reg<-predict(OLS.full1,newdata=val.reg, interval="predict")
pred.reg
View(val.reg)

#mean sq error 
mse1<-mean((val.reg$MonthlyCharge-pred.reg)^2)
mse1

actual_pred_OLS.full1=data.frame(val.reg$MonthlyCharge,pred.reg)


#logistic reg
train.logit<-train[,c(1:4,6:11)]
val.logit<-val[,c(1:4,6:11)]
View(train.logit)
str(train.logit)
logit<-glm(Churn~MonthlyCharge+AccountWeeks+ContractRenewal+CustServCalls
           +DayMins+DayCalls+OverageFee+RoamMins+DataPlan, data=train.logit, 
           family=binomial())
summary(logit)
glm_pred=fitted(logit)
glmreg_actual_pred<-data.frame(Actual=train.logit$Churn,Predicted=glm_pred)
View(glmreg_actual_pred)

#check goodness of fir using Pseudo r2
install.packages('pscl')
library(pscl)
pR2(logit)
#the independent var explain 19.38% of dependent var(McFadden value)
logit<-glm(Churn~MonthlyCharge+AccountWeeks+ContractRenewal+CustServCalls
           +DayMins+DayCalls+OverageFee+RoamMins+DataPlan, data=train.logit, 
           family=binomial())
#visual pattern for cont. var
pairs(cell[,c(2,5,7,8,9,10,11)])

ct.data<-subset(cell,select=c(ContractRenewal,DataPlan))
num.data<-subset(cell,select=c(AccountWeeks,DataUsage,CustServCalls,DayMins,DayCalls,MonthlyCharge,OverageFee,RoamMins))

par(mfrow=c(2,3))
for(i in names(ct.data)){
  print(i)
  print(table(cell$Churn,ct.data[[i]]))
  barplot(table(cell$Churn,ct.data[[i]]),
          col=c('grey','red'),main=names(ct.data[i]))
}

#chi sq test of caegorical var. with target
#bind target var to the categorical data and convert it to a factor

ct.data2<-cbind(cell$Churn,ct.data)
colnames(ct.data2)[1]<-'Churn'
ct.data2$Churn<-as.factor(ct.data2$Churn)
str(ct.data2)
#convert all var into factor
fact_ct.data2=ct.data2%>%mutate_if(is.integer,funs(factor(.)))
str(fact_ct.data2)

#perform chi sq
ChiSqStat<-NA

for(i in 1 :(ncol(fact_ct.data2))){
  Statistic<-data.frame(
    'Row'=colnames(fact_ct.data2[1]),
    'Column'=colnames(fact_ct.data2[i]),
    'Chi Square'=chisq.test(fact_ct.data2[[1]],fact_ct.data2[[i]])$statistic,
    'df'=chisq.test(fact_ct.data2[[1]],fact_ct.data2[[i]])$parameter,
    'p.value'=chisq.test(fact_ct.data2[[1]],fact_ct.data2[[i]])$p.value)
  ChiSqStat<-rbind(ChiSqStat,Statistic)
}
ChiSqStat<-data.table::data.table(ChiSqStat)
ChiSqStat

#running a sample model with factor var alone

fact.model<-glm(Churn~.,data=fact_ct.data2,family = binomial)
summary(fact.model)

#AC=2552= determines how good the model is

##working on numericaal variables
##boxplots of all numerical variables
library(RColorBrewer)
boxplot(num.data,las=1,horizontal=TRUE,cex=0.8,par(cex.axis=0.8),col=brewer.pal(8,'Set1'),main='boxplot of continuous variables')

#add churn to the dataset
num.data2=cbind(cell$Churn,num.data)
colnames(num.data2)[1]<-'Churn'
num.data2$Churn=as.factor(num.data2$Churn)

#build univariate logistic reg. models
##AccountWeeks,DataUsage,CustServCalls,DayMins,DayCalls,MonthlyCharge,OverageFee,RoamMins
mod.num<-glm(Churn~AccountWeeks,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~DataUsage,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~CustServCalls,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~DayMins,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~DayCalls,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~MonthlyCharge,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~OverageFee,data=num.data2,family=binomial)
summary(mod.num)

mod.num<-glm(Churn~RoamMins,data=num.data2,family=binomial)
summary(mod.num)

#removing account week and day calls freom the data set
names(num.data2)
num.data2<-subset(num.data2,select= -c(AccountWeeks,DayCalls,Churn))#Churn is removed as it is coming twice from fact_ct.data2 and num.data2
full.data<-cbind(num.data2,fact_ct.data2)
names(full.data)
full.data<-full.data[,c(7,1,2,3,4,5,6,8,9)]
names(full.data)
set.seed(300)

library(caTools)
spl=sample.split(full.data$Churn,SplitRatio = 0.65)
train=subset(full.data,spl==T)
test=subset(full.data,spl==F)
#chk % of the sample
sum(as.integer(as.character(train$Churn)))/nrow(train)
sum(as.integer(as.character(test$Churn)))/nrow(test)
sum(as.integer(as.character(full.data$Churn)))/nrow(full.data)

#build model
LRmodel=glm(Churn~.,data=train,family = binomial)
summary(LRmodel)

#check for multicollinearity
vif(LRmodel)
# there is high multicollinearity between the variables, hence removing dataplan and data usage as 74 and 78% of data s explained by monthly charge

full.data1<-subset(full.data,select=-c(DataUsage,DataPlan))
names(full.data1)

#checking again after removng data usage

set.seed(300)
library(caTools)
spl=sample.split(full.data1$Churn,SplitRatio = 0.65)
train=subset(full.data1,spl==T)
test=subset(full.data1,spl==F)
#chk % of the sample
sum(as.integer(as.character(train$Churn)))/nrow(train)
sum(as.integer(as.character(test$Churn)))/nrow(test)
sum(as.integer(as.character(full.data1$Churn)))/nrow(full.data1)

#build model
LRmodel=glm(Churn~.,data=train,family = binomial)
summary(LRmodel)

#check for multicollinearity
vif(LRmodel)

#get the confidence intervals
confint(LRmodel)


############train###


###predict the response of the model using the test data

predTrain=predict(LRmodel,newdata=train,type='response')

#build confusion matrix; >0.5=true else false
conf_mat=table(train$Churn,predTrain>0.5)
conf_mat
#get accuracy by using the right classifiers
(conf_mat[1,1]+conf_mat[2,2])/nrow(na.omit(train))

#plot the ROC curve for calculating AUC
library(ROCR)
ROCRpred=prediction(predTrain,train$Churn)
as.numeric(performance(ROCRpred,'auc')@y.values)
perf_train=performance(ROCRpred,'tpr','fpr')
plot(perf_train,col='black',lty=2,lwd=2)
plot(perf_train,lwd=3,colorize=TRUE)

ks_train <- max(perf_train@y.values[[1]]- perf_train@x.values[[1]])
plot(perf_train,main=paste0('KS=',round(ks_train*100,1),'%'))
lines(x = c(0,1),y=c(0,1))


############################

#predict the response of the model using the test data
predTest=predict(LRmodel,newdata=test,type='response')

#build confusion matrix; >0.5=true else false
conf_mat=table(test$Churn,predTest>0.5)
conf_mat
#get accuracy by using the right classifiers
(conf_mat[1,1]+conf_mat[2,2])/nrow(na.omit(test))

#plot the ROC curve for calculating AUC
library(ROCR)
ROCRpred=prediction(predTest,test$Churn)
as.numeric(performance(ROCRpred,'auc')@y.values)
perf_test=performance(ROCRpred,'tpr','fpr')
plot(perf_test,col='black',lty=2,lwd=2)
plot(perf_test,lwd=3,colorize=TRUE)

ks_test <- max(perf_test@y.values[[1]]- perf_test@x.values[[1]])
plot(perf_test,main=paste0('KS=',round(ks_test*100,1),'%'))
lines(x = c(0,1),y=c(0,1))

#Gini
train$prob = predict(LRmodel,type='response')
#install.packages("ineq")
library(ineq)
ineq(train$RF.Score,"gini")
ineq(test$RF.Score,"gini")


#####################################################
#knn and nb#

str(full.data1)
#convert variables to num in order to use knn
full.data1$CustServCalls<-as.numeric(full.data1$CustServCalls)
full.data1$ContractRenewal<-as.numeric(full.data1$ContractRenewal)
str(full.data1)
View(full.data1)

tcnorm<-scale(full.data1[,-1])
tcnorm<-cbind(full.data1[,1],tcnorm)
colnames(tcnorm)[1]<-'CHURN'
View(tcnorm)
#convert to a data frame

df_tcnorm<-as.data.frame(tcnorm)
df_tcnorm$CHURN<-as.factor(df_tcnorm$CHURN)

#check number values currently1=benign;2=malignant
table(df_tcnorm$CHURN)

#partition the data
library(caTools)
spl=sample.split(df_tcnorm,SplitRatio = 0.65)
train=subset(df_tcnorm, spl==T)
test=subset(df_tcnorm, spl==F)

#train using knn

library(class)
sqrt(nrow(train))
knn_model<- knn(train[-1],test[-1],train[,1],k=43)##works for test

#check confusion matrix
table.knn=table(test[,1],knn_model)
table.knn

#check accuracy
sum(diag(table.knn)/sum(table.knn))

#ch loss
loss.knn<-table.knn[2,1]/(table.knn[2,1]+table.knn[1,1])
loss.knn
opp.loss.knn<-table.knn[1,2]/(table.knn[1,2]+table.knn[2,2])
opp.loss.knn
tot.loss.knn<-0.95*loss.knn+0.05*opp.loss.knn
tot.loss.knn

#apply Naive Bayes
install.packages('e101')
library(e101)
nb_model=naiveBayes(CHURN~.,data=train1)

#apply predict function to see the performance
pred_nb=predict(nb_model,test,type='class')

#confusion matrix
tab.NB=table(test[,1],pred_nb)
tab.NB

#check accuracy
sum(diag(tab.NB)/sum(tab.NB))
#check loss
loss.NB<-tab.NB[2,1]/(tab.NB[2,1]+tab.NB[1,1])
loss.NB
opp.loss.NB<-tab.NB[1,2]/(tab.NB[1,2]+tab.NB[2,2])
opp.loss.NB
tot.loss.NB<-0.95*loss.NB+0.05*opp.loss.NB
tot.loss.NB

