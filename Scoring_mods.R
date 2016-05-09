data <- read.table(file = file.choose(), header = TRUE, sep = ";")
#decision tree
data$Gender <-as.factor(data$Gender)
data$age<-as.numeric(data$age)
data$Adress<-as.factor(data$Adress)
data$Status<-as.factor(data$Status)
data$BeginDate<-as.factor(data$BeginDate)
data$SumK<-as.numeric(data$SumK)
data$Term<-as.numeric(data$Term)
data$Rate<-as.numeric(data$Rate)
data$Delay<-as.factor(data$Delay)
str(data)
library(rpart)
model_tr<-rpart(Delay~., method='class', data, cp=0.001)
plot(model_tr)
text(model_tr) 
library(pROC) #library for roc-curve
roc_tr<-roc(data$Delay,predict(model_tr, type="prob"),ci=TRUE)
plot.roc(roc_tr)


#Logit
model_log <- glm(Delay ~Gender+age+SumK+Rate,family=binomial(link='logit'),data=data)
summary(model_log)
roc_log<-roc(data$Delay,predict(model_log, type="response"),ci=TRUE)
plot.roc(roc_log)

library(survival)
read.csv2("D:/Data/Autodata_Surv.csv")
data2<-read.csv2("D:/Data/Autodata_Surv.csv")
#Time - time of delay, Delay (0/1- default-nondefault)
data2$SurvObj <- with(data2, Surv(Time, Delay == 1))
res.cox1 <- coxph(SurvObj ~ Gender+age+SumK+Term+Rate, data = data2)
summary(res.cox1)
roc_cox<-roc(data2$Delay,predict(res.cox1, type="risk"),ci=TRUE)
plot(roc_cox)

