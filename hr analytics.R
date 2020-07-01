library(googleVis)
library(ggplot2)
library(caret)
library(MASS)
library(randomForest)

hr<-read.csv("C:/Users/Keerthivasan/Desktop/Data Analytics/HR_Data.csv")
sum(is.na(hr))
View(hr)
hr$left<-as.factor(hr$left)
hr$salary<-as.factor(hr$salary)

bp1<-ggplot(hr,aes(x=left,y=time_spend_company,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp1+facet_wrap(.~sales,scales='fixed',ncol=5)

bp2<-ggplot(hr,aes(x=left,y=satisfaction_level,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp2+facet_wrap(.~sales,scales='fixed',ncol=5)

bp3<-ggplot(hr,aes(x=left,y=last_evaluation,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp3+facet_wrap(.~sales,scales='fixed',ncol=5)

bp4<-ggplot(hr,aes(x=left,y=number_project,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp4+facet_wrap(.~sales,scales='fixed',ncol=5)

bp5<-ggplot(hr,aes(x=left,y=average_montly_hours,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp5+facet_wrap(.~sales,scales='fixed',ncol=5)

bp6<-ggplot(hr,aes(x=left,y=Work_accident,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp6+facet_wrap(.~sales,scales='fixed',ncol=5)

bp7<-ggplot(hr,aes(x=left,y=promotion_last_5years,group=interaction(left,salary),color=salary))+geom_boxplot(aes(fill=left))
bp7+facet_wrap(.~sales,scales='fixed',ncol=5)


set.seed(20)
test <- hr[rbinom(20, 10, 0.5),]
inTrain <- createDataPartition(hr$left, p=0.6, list=FALSE)

training.data <- hr[inTrain,]
testing.data <- hr[-inTrain,]
dim(training.data)
dim(testing.data)


#Logistic Rergression
mod_LR<-glm(data=hr,left~(satisfaction_level+last_evaluation+time_spend_company+number_project
                          +average_montly_hours+Work_accident+promotion_last_5years+sales+salary),family=binomial)
coef(summary(mod_LR))
mod_LR_exp <- coef(summary(mod_LR))
mod_LR_exp[, "Estimate"] <- exp(coef(mod_LR))
mod_LR_exp

pred_LR<-predict(mod_LR,testing.data)
pred_LR<-replace(pred_LR,pred_LR<0.5,0)
pred_LR<-replace(pred_LR,pred_LR>0.5,1)
confusionMatrix(as.factor(pred_LR),as.factor(testing.data$left))

#Linear Discriminant Analysis
modlda <- train(left ~., data=training.data, method="lda")
plda <- predict(modlda, testing.data)
confusionMatrix(as.factor(plda), as.factor(testing.data$left))


#GB Regression Model
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 1)

mod_BR <- train(left ~., training.data, method="gbm", trControl=fitControl, verbose = FALSE)
plot(mod_BR, main = "Model 2")
predict_BR <- predict(mod_BR, testing.data)
confusionMatrix(predict_BR, testing.data$left)


#Random Forest
mod_RF <- randomForest(left ~ ., training.data)
plot(mod_RF, main = "Model 3")
pred_RF <- predict(mod_RF, testing.data, type="class")
confusionMatrix(pred_RF, testing.data$left)


#Final Prediction
p <- predict(mod_RF, test)
head(p, 5)

