source("/home/subhabrata/CUSB/Project/R2p.R")

library(broom)

fdata<-read.csv("/home/subhabrata/CUSB/Project/foottot.csv")
attach(fdata)

dep<-as.matrix(fdata[c(12,13,21,22,23)])
indep<-as.matrix(fdata[-c(1,12,13,21,22,23)])




out1=list()
for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[j],"; Response: ",colnames(dep)[1])
mod<-lm(dep[,1]~indep[,j])
out1[[j]]<-list(nam,R2p(mod))
}

out2=list()
for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,j])
out2[[j]]<-list(nam,R2p(mod))
}


outv1<-list(out1,out2)


outv21=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],"and",colnames(indep)[j],"; Response: ",colnames(dep)[1])
mod<-lm(dep[,1]~indep[,11]+indep[,j])
outv21[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv1[[1]][[11]][[2]][2,2])<as.numeric(outv21[[i]][[2]][2,2]) && as.numeric(outv21[[i]][[2]][2,3])<=0.05)
{
print(outv1[[1]][[11]][[2]])
print(outv21[[i]])
}




