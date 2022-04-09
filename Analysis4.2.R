library(ggplot2)

#GoalScored
df<-data.frame(x<-c("Team", 'Manager', 'Height', 'Weight', 'BMI', 'Age', 'SubOn', 'RedCard', 'YellowCard', 'FoulsDrawn', 'FoulsCommited', 'Offside', 'Injury'), 
y=c(0.4846, 0.49, 0.4511, 0.4565, 0.4471, 0.4585, 0.4427, 0.4433, 0.4473, 0.4508, 0.4478, 0.4437, 0.4678))
                
ggplot(df, aes(x=y, y=x)) +
geom_segment(aes(x=0.4, xend=y,y=x,yend=x)) +
geom_point(size=5, color='red', fill=alpha("orange",0.3), alpha=0.7, shape=21, stroke=2) +
geom_text(aes(label=y),hjust=1.5,vjust=0) + 
labs(x="R^2",y="Variables that combined with MinutesPlayed")


#AssistwGoal
df<-data.frame(x<-c("Team", 'Manager', 'Height', 'Weight', 'Ligue', 'Age', 'SubOn', 'Nationality', 'YellowCard', 'FoulsCommited', 'Offside', 'Injury'), 
y=c(0.4448, 0.4524, 0.4228, 0.4014, 0.377, 0.3758, 0.3757, 0.3781, 0.3889, 0.4223, 0.4113, 0.3897))
                
ggplot(df, aes(x=y, y=x)) +
geom_segment(aes(x=0.35, xend=y,y=x,yend=x)) +
geom_point(size=5, color='red', fill=alpha("orange",0.3), alpha=0.7, shape=21, stroke=2) +
geom_text(aes(label=y),hjust=1.5,vjust=0) + 
labs(x="R^2",y="Variables that combined with MinutesPlayed")


#shotsPerGame
df<-data.frame(x<-c("Team", 'Manager', 'Nationality', 'Weight', 'BMI', 'Age', 'SubOn', 'RedCard', 'YellowCard', 'FoulsDrawn', 'Offside', 'Injury'), 
y=c(0.34, 0.3404, 0.3074, 0.3095, 0.3083, 0.3117, 0.3496, 0.3097, 0.3076, 0.3088, 0.3096, 0.3531))
                
ggplot(df, aes(x=y, y=x)) +
geom_segment(aes(x=0.3, xend=y,y=x,yend=x)) +
geom_point(size=5, color='red', fill=alpha("orange",0.3), alpha=0.7, shape=21, stroke=2) +
geom_text(aes(label=y),hjust=1.5,vjust=0) + 
labs(x="R^2",y="Variables that combined with MinutesPlayed")


#aerialWonPerGame
df<-data.frame(x<-c("Team", 'Manager', 'Ligue', 'BMI', 'Age', 'SubOn', 'MinutesPlayed', 'YellowCard', 'FoulsDrawn', 'FoulsCommited','Offside'), 
y=c(0.3468, 0.3518, 0.3433, 0.339, 0.3422, 0.3444, 0.343, 0.3579, 0.3544, 0.446, 0.4036))
                
ggplot(df, aes(x=y, y=x)) +
geom_segment(aes(x=0.3, xend=y,y=x,yend=x)) +
geom_point(size=5, color='red', fill=alpha("orange",0.3), alpha=0.7, shape=21, stroke=2) +
geom_text(aes(label=y),hjust=1.5,vjust=0) + 
labs(x="R^2",y="Variables that combined with Height")

