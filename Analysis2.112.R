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
nam<-paste("Predictor: ",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,j])
out2[[j]]<-list(nam,R2p(mod))
}


outv1<-list(out1,out2)

############################################################################
    
outv23=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,j])
outv23[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv1[[2]][[11]][[2]][2,2])<as.numeric(outv23[[i]][[2]][2,2]) && as.numeric(outv23[[i]][[2]][2,3])<=0.05)
{
print(outv23[[i]])
}

############################################################################

outv33=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,j])
outv33[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv23[[17]][[2]][2,2])<as.numeric(outv33[[i]][[2]][2,2]) && as.numeric(outv33[[i]][[2]][2,3])<=0.05)
{
print(outv33[[i]])
}

###########################################################################

outv43=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep )[17],",",colnames(indep)[10],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,j])
outv43[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv33[[10]][[2]][2,2])<as.numeric(outv43[[i]][[2]][2,2]) && as.numeric(outv43[[i]][[2]][2,3])<=0.05)
{
print(outv43[[i]])
}

##########################################################################

outv53=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,j])
outv53[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv43[[1]][[2]][2,2])<as.numeric(outv53[[i]][[2]][2,2]) && as.numeric(outv53[[i]][[2]][2,3])<=0.05)
{
print(outv53[[i]])
}

##########################################################################

outv63=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,j])
outv63[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv53[[3]][[2]][2,2])<as.numeric(outv63[[i]][[2]][2,2]) && as.numeric(outv63[[i]][[2]][2,3])<=0.05)
{
print(outv63[[i]])
}

############################################################################

outv73=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[16],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,16]+indep[,j])
outv73[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv63[[16]][[2]][2,2])<as.numeric(outv73[[i]][[2]][2,2]) && as.numeric(outv73[[i]][[2]][2,3])<=0.05)
print(outv73[[i]])

##########################################################################

outv83=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[16],",",colnames(indep)[4],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,16]+indep[,4]+indep[,j])
outv83[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv73[[4]][[2]][2,2])<as.numeric(outv83[[i]][[2]][2,2]) && as.numeric(outv83[[i]][[2]][2,3])<=0.05)
print(outv83[[i]])

#########################################################################

outv93=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[16],",",colnames(indep)[4],",",colnames(indep)[2],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,16]+indep[,4]+indep[,2]+indep[,j])
outv93[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv83[[2]][[2]][2,2])<as.numeric(outv93[[i]][[2]][2,2]) && as.numeric(outv93[[i]][[2]][2,3])<=0.05)
print(outv93[[i]])

#########################################################################


outv103=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[16],",",colnames(indep)[4],",",colnames(indep)[13],",",colnames(indep)[2],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,16]+indep[,4]+indep[,13]+indep[,2]+indep[,j])
outv103[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv93[[13]][[2]][2,2])<as.numeric(outv103[[i]][[2]][2,2]) && as.numeric(outv103[[i]][[2]][2,3])<=0.05)
print(outv103[[i]])

#########################################################################

outv113=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[17],",",colnames(indep)[10],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[16],",",colnames(indep)[4],",",colnames(indep)[13],",",colnames(indep)[2],",",colnames(indep)[8],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,3]~indep[,11]+indep[,17]+indep[,10]+indep[,1]+indep[,3]+indep[,16]+indep[,4]+indep[,13]+indep[,2]+indep[,8]+indep[,j])
outv113[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv103[[8]][[2]][2,2])<as.numeric(outv113[[i]][[2]][2,2]) && as.numeric(outv112[[i]][[2]][2,3])<=0.05)
print(outv113[[i]])

########################################################################
########################################################################










