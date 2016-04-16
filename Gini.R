You need to sort table decreasing by score. The ranged row of numbers is a probabilities to become 1! The lower the score - the less 
probability to become default (in this case 1)
data_1 <- read.table(file = file.choose(), header = TRUE, sep = ";")
data_sort<-data_1[order(-data_1$FACT),] 
data_sort<-data_1[order(data_1$FACT),] 
N=length(which(data_1$TAR==0))
P=length(which(data_1$TAR==1))
TPR<-c()
FPR<-c()
TP=0
FN=0
for (i in data_sort$TAR){
  if (i==1) {TP=TP+1} 
  else {FN=FN+1}
  TPR<-c(TPR, TP/P)
  FPR<-c(FPR, FN/N)
}

plot(FPR,TPR, type="l")
t<-data.frame(FPR,TPR)
s<-c()
j=0
num=nrow(t)-1
for (j in 1:num){
  s[j]=(FPR[j]+FPR[j+1])*(TPR[j+1]-TPR[j])/2
}
AUC=sum(s)
AUC
Gini=2*(AUC-0.5)
Gini

