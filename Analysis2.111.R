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

############################################################################
############################################################################

outv22=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,j])
outv22[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv1[[2]][[11]][[2]][2,2])<as.numeric(outv22[[i]][[2]][2,2]) && as.numeric(outv22[[i]][[2]][2,3])<=0.05)
{
print(outv22[[i]])
}

############################################################################
############################################################################

outv32=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,j])
outv32[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv22[[2]][[2]][2,2])<as.numeric(outv32[[i]][[2]][2,2]) && as.numeric(outv32[[i]][[2]][2,3])<=0.05)
{
print(outv32[[i]])
}

###########################################################################
###########################################################################

outv42=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,j])
outv42[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv32[[5]][[2]][2,2])<as.numeric(outv42[[i]][[2]][2,2]) && as.numeric(outv42[[i]][[2]][2,3])<=0.05)
{
print(outv42[[i]])
}

##########################################################################
##########################################################################

outv52=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,j])
outv52[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv42[[15]][[2]][2,2])<as.numeric(outv52[[i]][[2]][2,2]) && as.numeric(outv52[[i]][[2]][2,3])<=0.05)
{
print(outv52[[i]])
}

##########################################################################
##########################################################################

outv62=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,j])
outv62[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv52[[3]][[2]][2,2])<as.numeric(outv62[[i]][[2]][2,2]) && as.numeric(outv62[[i]][[2]][2,3])<=0.05)
{
print(outv62[[i]])
}

############################################################################
############################################################################


outv72=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],",",colnames(indep)[1],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,1]+indep[,j])
outv72[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv62[[1]][[2]][2,2])<as.numeric(outv72[[i]][[2]][2,2]) && as.numeric(outv72[[i]][[2]][2,3])<=0.05)
print(outv72[[i]])

##########################################################################
##########################################################################

outv82=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],",",colnames(indep)[1],",",colnames(indep)[10],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,1]+indep[,10]+indep[,j])
outv82[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
print(as.numeric(outv72[[10]][[2]][2,2])<as.numeric(outv82[[i]][[2]][2,2]) && as.numeric(outv82[[i]][[2]][2,3])<=0.05)
print(outv82[[i]])

#########################################################################
#########################################################################

outv92=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],",",colnames(indep)[1],",",colnames(indep)[10],",",colnames(indep)[13],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,1]+indep[,10]+indep[,13]+indep[,j])
outv92[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv82[[13]][[2]][2,2])<as.numeric(outv92[[i]][[2]][2,2]) && as.numeric(outv92[[i]][[2]][2,3])<=0.05)
print(outv92[[i]])

#########################################################################


outv102=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],",",colnames(indep)[1],",",colnames(indep)[10],",",colnames(indep)[13],",",colnames(indep)[8],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,1]+indep[,10]+indep[,13]+indep[,8]+indep[,j])
outv102[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
if(as.numeric(outv92[[8]][[2]][2,2])<as.numeric(outv102[[i]][[2]][2,2]) && as.numeric(outv102[[i]][[2]][2,3])<=0.05)
print(outv102[[i]])

#########################################################################
#########################################################################


outv112=list()

for(j in 1:ncol(indep))
{
nam<-paste("Predictor: ",colnames(indep)[11],",",colnames(indep)[2],",",colnames(indep)[5],",",colnames(indep)[15],",",colnames(indep)[3],",",colnames(indep)[1],",",colnames(indep)[10],",",colnames(indep)[13],",",colnames(indep)[8],",",colnames(indep)[16],"and",colnames(indep)[j],"; Response: ",colnames(dep)[2])
mod<-lm(dep[,2]~indep[,11]+indep[,2]+indep[,5]+indep[,15]+indep[,3]+indep[,1]+indep[,10]+indep[,13]+indep[,8]+indep[,16]+indep[,j])
outv112[[j]]=(list(nam,R2p(mod)))
}

for( i in 1:ncol(indep))
print(as.numeric(outv102[[16]][[2]][2,2])<as.numeric(outv112[[i]][[2]][2,2]) && as.numeric(outv112[[i]][[2]][2,3])<=0.05)
print(outv112[[i]])

########################################################################
########################################################################










