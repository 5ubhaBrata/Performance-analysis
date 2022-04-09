

nam<-paste("Predictor: ",colnames(indep)[2],",",colnames(indep)[8],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[13],",",colnames(indep)[16],",",colnames(indep)[11],",",colnames(indep)[10],",",colnames(indep)[14],",",colnames(indep)[5],"and","; Response: ",colnames(dep)[1])
mod1<-lm(dep[,1]~indep[,2]+indep[,8]+indep[,1]+indep[,3]+indep[,13]+indep[,16]+indep[,11]+indep[,10]+indep[,5]+indep[,15]+indep[,14]+indep[,17])
outv1f=(list(nam,R2p(mod1)))


nam<-paste("Predictor: ",colnames(indep)[2],",",colnames(indep)[8],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[13],",",colnames(indep)[16],",",colnames(indep)[11],",",colnames(indep)[10],",",colnames(indep)[14],",",colnames(indep)[5],"and","; Response: ",colnames(dep)[2])
mod2<-lm(dep[,2]~indep[,2]+indep[,8]+indep[,1]+indep[,3]+indep[,13]+indep[,16]+indep[,11]+indep[,10]+indep[,5]+indep[,15]+indep[,14]+indep[,17])
outv2f=(list(nam,R2p(mod2)))


nam<-paste("Predictor: ",colnames(indep)[2],",",colnames(indep)[8],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[13],",",colnames(indep)[16],",",colnames(indep)[11],",",colnames(indep)[10],",",colnames(indep)[14],",",colnames(indep)[5],"and","; Response: ",colnames(dep)[3])
mod3<-lm(dep[,3]~indep[,2]+indep[,8]+indep[,1]+indep[,3]+indep[,13]+indep[,16]+indep[,11]+indep[,10]+indep[,5]+indep[,15]+indep[,14]+indep[,17])
outv3f=(list(nam,R2p(mod3)))


nam<-paste("Predictor: ",colnames(indep)[2],",",colnames(indep)[8],",",colnames(indep)[1],",",colnames(indep)[3],",",colnames(indep)[13],",",colnames(indep)[16],",",colnames(indep)[11],",",colnames(indep)[10],",",colnames(indep)[14],",",colnames(indep)[5],"and","; Response: ",colnames(dep)[4])
mod4<-lm(dep[,4]~indep[,2]+indep[,8]+indep[,1]+indep[,3]+indep[,13]+indep[,16]+indep[,11]+indep[,10]+indep[,5]+indep[,15]+indep[,14]+indep[,17])
outv4f=(list(nam,R2p(mod4)))

