
fdata<-read.csv("/home/subhabrata/CUSB/Project/foottot.csv")
attach(fdata)

dep<-as.matrix(fdata[c(12,13,21,22,23)])
indep<-as.matrix(fdata[-c(1,12,13,21,22,23)])

GoalScoredPerGame<-GoalScored/MatchPlayed
AssistwGoalPerGame<-AssistwGoal/MatchPlayed

Performance<-3*GoalScoredPerGame+2*AssistwGoalPerGame+shotsPerGame+aerialWonPerGame

Performancetxt<-NULL
for(i in 1:length(Performance))
{
if(Performance[i]<fivenum(Performance)[2])
Performancetxt[i]<-"Okay"
if(Performance[i]>=fivenum(Performance)[2] && Performance[i]<fivenum(Performance)[3])
Performancetxt[i]<-"Good"
if(Performance[i]>=fivenum(Performance)[3] && Performance[i]<fivenum(Performance)[4])
Performancetxt[i]<-"Better"
if(Performance[i]>fivenum(Performance)[4])
Performancetxt[i]<-"Best"
}

Performtxt<-as.factor(Performancetxt)


vglm(Performtxt ~ MinutesPlayed, family=cumulative(parallel=TRUE))

