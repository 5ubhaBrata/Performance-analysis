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
nam<-paste("Predictor: ",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,j])
out2[[j]]<-list(nam,R2p(mod))
}


outv1<-list(out1,out2)

############################################################################
    
outv25=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,j])
outv25[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv1[[1]][[5]][[2]][2,2])<as.numeric(outv25[[i]][[2]][2,2]) && as.numeric(outv25[[i]][[2]][2,3])<=0.05)
{
print(outv25[[i]])
}

############################################################################
############################################################################

outv35=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,5]+indep[,j])
outv35[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv25[[5]][[2]][2,2])<as.numeric(outv35[[i]][[2]][2,2]) && as.numeric(outv35[[i]][[2]][2,3])<=0.05)
{
print(outv35[[i]])
}


###########################################################################

outv45=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep )[5],",",colnames(indep)[15],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,5]+indep[,15]+indep[,j])
outv45[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv35[[15]][[2]][2,2])<as.numeric(outv45[[i]][[2]][2,2]) && as.numeric(outv45[[i]][[2]][2,3])<=0.05)
{
print(outv45[[i]])
}

##########################################################################

outv55=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[14],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,5]+indep[,15]+indep[,14]+indep[,j])
outv55[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv45[[14]][[2]][2,2])<as.numeric(outv55[[i]][[2]][2,2]) && as.numeric(outv55[[i]][[2]][2,3])<=0.05)
{
print(outv55[[i]])
}

##########################################################################

outv65=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[14],",",colnames(indep)[10],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,5]+indep[,15]+indep[,14]+indep[,10]+indep[,j])
outv65[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv55[[10]][[2]][2,2])<as.numeric(outv65[[i]][[2]][2,2]) && as.numeric(outv65[[i]][[2]][2,3])<=0.05)
{
print(outv65[[i]])
}

############################################################################

outv75=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[14],",",colnames(indep)[10],",",colnames(indep)[9],"and",colnames(indep)[j],"; Response: ",colnames(dep)[5])
mod<-lm(dep[,5]~indep[,1]+indep[,5]+indep[,15]+indep[,14]+indep[,10]+indep[,9]+indep[,j])
outv75[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv65[[9]][[2]][2,2])<as.numeric(outv75[[i]][[2]][2,2]) && as.numeric(outv75[[i]][[2]][2,3])<=0.05)
print(outv75[[i]])

##########################################################################

outv84=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[8],",",colnames(indep)[3],",",colnames(indep)[9],",",colnames(indep)[13],"and",colnames(indep)[j],"; Response: ",colnames(dep)[4])
mod<-lm(dep[,4]~indep[,1]+indep[,5]+indep[,15]+indep[,8]+indep[,3]+indep[,9]+indep[,13]+indep[,j])
outv84[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv74[[13]][[2]][2,2])<as.numeric(outv84[[i]][[2]][2,2]) && as.numeric(outv84[[i]][[2]][2,3])<=0.05)
print(outv84[[i]])

#########################################################################

outv94=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[8],",",colnames(indep)[3],",",colnames(indep)[9],",",colnames(indep)[13],",",colnames(indep)[17],"and",colnames(indep)[j],"; Response: ",colnames(dep)[3])
mod<-lm(dep[,4]~indep[,1]+indep[,5]+indep[,15]+indep[,8]+indep[,3]+indep[,9]+indep[,17]+indep[,13]+indep[,j])
outv94[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv84[[17]][[2]][2,2])<as.numeric(outv94[[i]][[2]][2,2]) && as.numeric(outv94[[i]][[2]][2,3])<=0.05)
print(outv94[[i]])

#########################################################################


outv104=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[8],",",colnames(indep)[3],",",colnames(indep)[9],",",colnames(indep)[13],",",colnames(indep)[17],",",colnames(indep)[14],"and",colnames(indep)[j],"; Response: ",colnames(dep)[4])
mod<-lm(dep[,4]~indep[,1]+indep[,5]+indep[,15]+indep[,8]+indep[,3]+indep[,9]+indep[,13]+indep[,17]+indep[,14]+indep[,j])
outv104[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv94[[14]][[2]][2,2])<as.numeric(outv104[[i]][[2]][2,2]) && as.numeric(outv104[[i]][[2]][2,3])<=0.05)
print(outv104[[i]])

#########################################################################

outv114=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[1],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[8],",",colnames(indep)[3],",",colnames(indep)[9],",",colnames(indep)[13],",",colnames(indep)[17],",",colnames(indep)[14],",",colnames(indep)[1],"and",colnames(indep)[j],"; Response: ",colnames(dep)[4])
mod<-lm(dep[,4]~indep[,1]+indep[,5]+indep[,15]+indep[,8]+indep[,3]+indep[,9]+indep[,13]+indep[,17]+indep[,14]+indep[,1]+indep[,j])
outv114[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv104[[1]][[2]][2,2])<as.numeric(outv114[[i]][[2]][2,2]) && as.numeric(outv114[[i]][[2]][2,3])<=0.05)
print(outv114[[i]])

########################################################################
########################################################################










