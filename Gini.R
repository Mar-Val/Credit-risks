#Ran-is a vector of characteristics, Tar - is a vector of zeroes-ones, dir-direction of sorting.
#You need to sort table decreasing by score.
data_2 <- read.table(file = file.choose(), header = TRUE, sep = ";")



Gini_MarVal <-function (Ran,Tar,dir) {
data_1<-data.frame(Ran,Tar)
data_sort<-data_1[order(dir*data_1$Ran),] 
data_sort
N=length(which(data_1$Tar==0))
P=length(which(data_1$Tar==1))
TPR<-c()
FPR<-c()
TP=0
FN=0
for (i in data_sort$Tar){
  if (i==1) {TP=TP+1} 
  else {FN=FN+1}
  TPR<-c(TPR, TP/P)
  FPR<-c(FPR, FN/N)
}

TPR
FPR
plot(FPR,TPR, type="l",col="red",xlab="FPR",ylab="TPR")
par(new=TRUE)
plot(c(0,1),c(0,1),type="l",axes=FALSE,xlab="FPR",ylab="TPR")
t<-data.frame(FPR,TPR)
s<-c()
j=0
num=nrow(t)-1
for (j in 1:num){
  s[j]=(FPR[j+1]-FPR[j])*(TPR[j+1]+TPR[j])/2
}
AUC=sum(s)
Gini
Gini=2*(AUC-0.5)
return(Gini)
}

do.call("Gini_MarVal", list( Ran=data_2$R, Tar=data_2$T, dir=1))




