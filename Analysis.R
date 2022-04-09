source("/home/subhabrata/CUSB/Project/vif.R")


library(corrplot)


fdata<-read.csv("/home/subhabrata/CUSB/Project/foottot.csv")
attach(fdata)

####### correlation between MinutesPlayed, GoalScored, AssistwGoal, shotsPerGame, aerialWonPerGame
corr<-cor(data.frame(MinutesPlayed, GoalScored, AssistwGoal, shotsPerGame, aerialWonPerGame))
corrplot.mixed(corr,tl.pos="lt")

####### correlation between Height, GoalScored, AssistwGoal, shotsPerGame, aerialWonPerGame
corr<-cor(data.frame(Height, GoalScored, AssistwGoal, shotsPerGame, aerialWonPerGame))
corrplot.mixed(corr,tl.pos="lt")


####### correlation between all indepdent variable
corr<-cor(fdata[-c(1,12,13,21,22,23)])
corrplot.mixed(corr,tl.pos="lt")



######### VIF for each response variable
par(mfrow=c(2,2))

M<-lm(GoalScored~.,data=fdata[-c(1,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for GoalScored")
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")
legend("topright",title="VIF Values", lty=2, lwd=3, legend=c(10,5,4), col=c("red","gold","green"))


M<-lm(AssistwGoal~.,data=fdata[-c(1,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for AssistwGoal")
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")


M<-lm(shotsPerGame~.,data=fdata[-c(1,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for shotsPerGame")
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")

M<-lm(aerialWonPerGame~.,data=fdata[-c(1,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for aerialWonPerGame")
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")



####### VIF after droping Weight

M<-lm(GoalScored~.,data=fdata[-c(1,7,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for GoalScored")
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")
legend("topright",title="VIF Values", lty=2, lwd=3, legend=c(10,5,4), col=c("red","gold","green"))

####### VIF after droping Weight and MatchPlayed

M<-lm(GoalScored~.,data=fdata[-c(1,7,10,12,13,21,22,23)])
vif_values <- vif(M)
barplot(vif_values, col = "steelblue",ylab='VIF values',main="VIF for GoalScored",ylim=c(0,5.5))
abline(h = 10, lwd = 3, lty = 2,col="red")
abline(h = 5, lwd = 3, lty = 2,col="gold")
abline(h = 4, lwd = 3, lty = 2,col="green")
legend(list(x=0,y=5),title="VIF Values", lty=2, lwd=3, legend=c(10,5,4), col=c("red","gold","green"))




mod1<-lm(GoalScored~BMI)
summary(mod1)
anova(mod1)



corr<-cor(fdata[c(14,3,7,9,11,2,4,16,19,17,18,20)])
corrplot.mixed(corr,tl.pos="lt")


M<-lm(GoalScored~.,data=fdata[c(14,3,7,9,11,2,4,16,19,17,18,20)])
vif_values <- vif(M)
barplot(vif_values, main = "VIF Values", col = "steelblue")
abline(h = 5, lwd = 3, lty = 2,col="red")
abline(h = 4, lwd = 3, lty = 2,col="gold")
abline(h = 3, lwd = 3, lty = 2,col="green")























