library(reshape)
library(reshape2)
library(plyr)

# groupA<-data.frame(game=c(1,1,2,2,3,3,4,4,5,5,6,6),team=c(1,2,3,4,1,3,2,4,1,4,2,3))
# groupA$goals<-c(rpois(8,1),rep(NA,4))

cd<-"C:/Users/Sergio/Documents/worldcup"
groupA<-read.csv(paste(cd,"groupA.csv", sep="/"))
groupA$group<-"A"

master<-read.csv(paste(cd,"master.csv",sep="/"))

gothru<-function(grp) {
  grp$teamingame<-rep(c(1,2),6)
  grp.wide<-dcast(grp,game~teamingame,value.var="goals")
  names(grp.wide)<-c("game","g1","g2")
  grp.wide$gd1<-grp.wide$g1-grp.wide$g2
  grp.wide$gd2<-grp.wide$g2-grp.wide$g1
  grp.wide<-subset(grp.wide,select=c(game,gd1,gd2))
  grp.gd<-melt(grp.wide,id=c("game"))
  grp.gd$variable<-substring(grp.gd$variable,3,4)
  names(grp.gd)<-c("game","teamingame","gd")
  
  grp.all<-merge(grp,grp.gd)
  grp.all$pts<-sign(grp.all$gd)+1+as.numeric(grp.all$gd>0)
  
  rankings<-ddply(grp.all,"team",summarize,points=sum(pts,na.rm=TRUE),gd=sum(gd,na.rm=TRUE),gf=sum(goals,na.rm=TRUE))
  rankings<-rankings[order(rankings$points,rankings$gd,rankings$gf,decreasing=TRUE),]
  thru<-rankings$team[1:2]
  return(thru)
}
gothru(groupA)

# To get result for 2 outcomes: sapply(list(groupA,groupA2),gothru)

simulate <- function (scores) {
  grp<-group
  grp$goals[is.na(grp$goals)]<-scores
  return(gothru(grp))
}

group<-groupA
simulation<-expand.grid(g1=c(0:5),g2=c(0:5),g3=c(0:5),g4=c(0:5))
results<-apply(simulation,1,simulate)
simulation$first<-results[1,]
simulation$second<-results[2,]

simulation$score1<-sign(simulation$g1-simulation$g2)
simulation$score2<-sign(simulation$g3-simulation$g4)


Mode <- function(vec) {
  all<-c(1,2,3,4)
  all[which.max(tabulate(match(vec,all)))]
}
only<-function(vec) {
  return(min(vec==Mode(vec)))
}
summary<-ddply(simulation,c("score1","score2"),summarize,firstmd=Mode(first),firstonly=only(first),secondmd=Mode(second),secondonly=only(second))
summary
