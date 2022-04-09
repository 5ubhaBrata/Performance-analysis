library(fmsb)

#Team
df<-data.frame(GoalScored=c(1,0,0.155),AssistwGoal=c(1,0,0.1884),
                shotsPerGame=c(1,0,0.1162),aerialWonPerGame=c(1,0,0.0157))
                
radarchart(df,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 1 : R-square for "Team"'
    )

#Manager
df1<-data.frame(GoalScored=c(1,0,0.1668),AssistwGoal=c(1,0,0.2023),
                shotsPerGame=c(1,0,0.1186),aerialWonPerGame=c(1,0,0.0359))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 2 : R-square for "Manager"'  
    )
    
#Ligue
df1<-data.frame(GoalScored=c(1,0,0.0011),AssistwGoal=c(1,0,0.0),
                shotsPerGame=c(1,0,0.0015),aerialWonPerGame=c(1,0,0.0052))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 3 : R-square for "Ligue"'  
    )

#Nationality
df1<-data.frame(GoalScored=c(1,0,0.0001),AssistwGoal=c(1,0,0.0092),
                shotsPerGame=c(1,0,0.0001),aerialWonPerGame=c(1,0,0.0077))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 4 : R-square for "Nationality"'  
    )

#Height
df1<-data.frame(GoalScored=c(1,0,0.0152),AssistwGoal=c(1,0,0.0414),
                shotsPerGame=c(1,0,0.0027),aerialWonPerGame=c(1,0,0.3375))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 5 : R-square for "Height"'  
    )

#Weight
df1<-data.frame(GoalScored=c(1,0,0.0323),AssistwGoal=c(1,0,0.017),
                shotsPerGame=c(1,0,0.0111),aerialWonPerGame=c(1,0,0.2034))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 6 : R-square for "Weight"'  
    )

#BMI
df1<-data.frame(GoalScored=c(1,0,0.0137),AssistwGoal=c(1,0,0.002),
                shotsPerGame=c(1,0,0.0086),aerialWonPerGame=c(1,0,0.0002))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 7 : R-square for "BMI"'  
    )

#Age
df1<-data.frame(GoalScored=c(1,0,0.0285),AssistwGoal=c(1,0,0.0072),
                shotsPerGame=c(1,0,0.0131),aerialWonPerGame=c(1,0,0.0103))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 8 : R-square for "Age"'  
    )

#MatchPlayed
df1<-data.frame(GoalScored=c(1,0,0.2737),AssistwGoal=c(1,0,0.2493),
                shotsPerGame=c(1,0,0.1098),aerialWonPerGame=c(1,0,0.0012))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 9 : R-square for "MatchPlayed"'  
    )

#SubOn
df1<-data.frame(GoalScored=c(1,0,0.1037),AssistwGoal=c(1,0,0.0932),
                shotsPerGame=c(1,0,0.1815),aerialWonPerGame=c(1,0,0.0052))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 10 : R-square for "SubOn"'  
    )

#MinutesPlayed
df1<-data.frame(GoalScored=c(1,0,0.4407),AssistwGoal=c(1,0,0.3731),
                shotsPerGame=c(1,0,0.3044),aerialWonPerGame=c(1,0,0.0109))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 11 : R-square for "MinutesPlayed"'  
    )

#RedCard
df1<-data.frame(GoalScored=c(1,0,0.0003),AssistwGoal=c(1,0,0.0005),
                shotsPerGame=c(1,0,0.0763),aerialWonPerGame=c(1,0,0.0016))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "RedCard"'  
    )

#YellowCard
df1<-data.frame(GoalScored=c(1,0,0.0224),AssistwGoal=c(1,0,0.0081),
                shotsPerGame=c(1,0,0.0183),aerialWonPerGame=c(1,0,0.0229))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "YellowCard"'  
    )

#FoulsDrawn
df1<-data.frame(GoalScored=c(1,0,0.0815),AssistwGoal=c(1,0,0.0913),
                shotsPerGame=c(1,0,0.1308),aerialWonPerGame=c(1,0,0.01))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "FoulsDrawn"'  
    )

#FoulsCommited
df1<-data.frame(GoalScored=c(1,0,0.062),AssistwGoal=c(1,0,0.0106),
                shotsPerGame=c(1,0,0.0755),aerialWonPerGame=c(1,0,0.2049))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "FoulsCommited"'  
    )


#Offside
df1<-data.frame(GoalScored=c(1,0,0.1933),AssistwGoal=c(1,0,0.0429),
                shotsPerGame=c(1,0,0.1496),aerialWonPerGame=c(1,0,0.1396))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "Offside"'  
    )

#Injury
df1<-data.frame(GoalScored=c(1,0,0.0015),AssistwGoal=c(1,0,0.0032),
                shotsPerGame=c(1,0,0.0029),aerialWonPerGame=c(1,0,0.0008))
                
radarchart(df1,
    axistype=4,
    pcol='red',
    pfcol=rgb(0.9,0.2,0.5,0.3),
    plwd=3, 
    cglcol='black',
    cglty=1,
    axislabcol='gray',
    cglwd=0.6,
    vlcex=1.1,
    title='Fig 12 : R-square for "Injury"'  
    )




