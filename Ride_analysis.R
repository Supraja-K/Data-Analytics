setwd("C:/Users/KEERTHIVASAN/Downloads")

library(psych)
library(dplyr)
library(car)
library(corrplot)



ridedata=read.csv("RideData.csv")
attach(ridedata)
View(ridedata)

yr<-as.factor(yr)
mnth=as.factor(mnth)
hr=as.factor(hr)
holiday=as.factor(holiday)
weekday=as.factor(weekday)
workingday=as.factor(workingday)
weathersit=as.factor(weathersit)

summary(ridedata)
str(ridedata)
describe(ridedata)
dim(ridedata)
names(ridedata)
hist(temp,col="blue2")
plot(atemp,cnt,col="red")
abline(lm(cnt~atemp),col="blue")

#Removing the attribute instant as it is the index
ride=ridedata[-1]
ride=ride[-1]
#Removing the dependent variable 'cnt'
ride=ride[-17]
sum(is.na(ride))
describe(ride[-1])

par(mfrow = c(2,2))
for(i in c(1:4)){
  boxplot(ride[,i], cnt, col= 'blue',
        xlab = names(ride)[i],
        ylab = "No. of riders",)
}

boxplot(hum,col="red")

#ride1 consists of only continuous independent attributes
ride1=ridedata[11:17]
ride1=ride1[-5]
ride1=ride1[-5]

cor=cor(ride1[-5])
corrplot(cor,method="number")

refinedmodel=lm(cnt~.,ride1[-2])
summary(refinedmodel)



  rmodel=lm(cnt~.,ride[1]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[2]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[3]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[4]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[5]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[6]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[7]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[8]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[9]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[10]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[11]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[12]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[13]);
  summary(rmodel)
  rmodel=lm(cnt~.,ride[14]);
  summary(rmodel)

model=lm(cnt~.,ride) 
summary(model) 
vif(model)

ride2=ride[-5]
pca=principal(r=ride2[-1],rotate="varimax")
print(pca)

#Prediction
set.seed(1000)
ind = sample(2, nrow(ride), replace = TRUE, prob=c(0.65,0.35))
train.df = ride[ind == 1,]
validation.df = ride[ind == 2,]

Model1 <- glm(cnt ~ ., data = train.df)

split=sample(1:nrow(ride),0.7*nrow(ride))
traindata=ride[split,]
testdata=ride[-split,]
dim(traindata)
dim(testdata)
ride=ride[-5]
ride=r
regmodel=lm(cnt~.,traindata)
summary(regmodel)
vif(regmodel)

pred=predict(regmodel,newdata=testdata,type="response")
testdata$Satisfaction.predict<-pred
head(testdata[c(1,6)],10)

SSE=sum((testdata$Satisfaction-pred)^2)
SST=sum((testdata$Satisfaction-mean(testdata$Satisfaction))^2)
SSR=SST-SSE
Rsq=SSR/SST
t1=1-Rsq
t2=(30-1)/(30-4-1)
adjrsq=1-(t1*t2)
adjrsq

library(Metrics)
RMSE=sqrt(SSE/(30-4-1))
rmse(testdata$Satisfaction,pred)

