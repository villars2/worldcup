library(reshape)
library(reshape2)
library(plyr)
library(rjson)

# groupA<-data.frame(game=c(1,1,2,2,3,3,4,4,5,5,6,6),team=c(1,2,3,4,1,3,2,4,1,4,2,3))
# groupA$goals<-c(rpois(8,1),rep(NA,4))

### Set directory and load in scores 
#cd<-"C:/Users/Sergio/Documents/GitHub/worldcup"
#cd<-"C:/Users/sv2307/Documents/GitHub/worldcup"

##### Put functions here
### Pulling data from world cup API.
## Argument=1 is to pull file from API, 2 is to get file and save, 3 is to
## read file from filename (only body of name, no .csv)
buildgroups <- function (save=1,filename="groupPull") {
  if (save==1 || save==2) {
    json_file<-"http://worldcup.sfg.io/matches"
    json_data<-fromJSON(file=json_file)
    
    pullGame <- function(g_data) {
      g<-subset(subset(do.call("rbind.fill",lapply(g_data,as.data.frame)),country!="<NA>",select=c(country,code,goals)))
      g$g_id<-g_data[[1]]
      if (g_data[[4]]!="completed") {
        g$goals<-c(NA,NA)
      }
      return(g)
    }
    
    groupGames<-do.call("rbind.fill",lapply(lapply(json_data[1:48],pullGame),as.data.frame))
    names(groupGames)<-c("country","fifa_code","goals","game")
    
    teams_file<-"http://worldcup.sfg.io/teams"
    teams_data<-fromJSON(file=teams_file)
    teams_data<-lapply(teams_data, function(x) {
      x[sapply(x, is.null)] <- NA
      unlist(x)
    })
    teamsMaster<-as.data.frame(do.call("rbind", teams_data))
    teamsMaster$group<-LETTERS[teamsMaster$group_id]
    teamsMaster<-subset(teamsMaster,select=c(country,fifa_code,group))
    
    groupGames<-merge(groupGames,teamsMaster)
    groupGames<-groupGames[order(groupGames$group,groupGames$game,groupGames$country),]
  }
  
  if (save==2) {
    write.csv(groupGames,paste(getwd(),"/groupPull.csv",sep=""))
  }
  
  if (save==3) {
    groupGames<-read.csv(paste(getwd(),"/",filename,".csv", sep=""))
  }
  
  return(groupGames)
}

### Function to get top 2 teams (in order) in each group given scores
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
  
  rankings<-ddply(grp.all,"country",summarize,points=sum(pts,na.rm=TRUE),gd=sum(gd,na.rm=TRUE),gf=sum(goals,na.rm=TRUE))
  rankings<-rankings[order(rankings$points,rankings$gd,rankings$gf,decreasing=TRUE),]
  thru<-rankings$country[1:2]
  return(thru)
}

### Function to replace NA scores by simulated scores, and get top 2 teams
simulate <- function (scores) {
  grp<-group
  grp$goals[is.na(grp$goals)]<-scores
  return(gothru(grp))
}

### Compute qualifying teams given all possible scores
Mode <- function(vec) {
  all<-levels(vec)
  all[which.max(tabulate(match(vec,all)))]
}
only<-function(vec) {
  return(min(vec==Mode(vec)))
}

### Builds summary of outcomes given scores
getsummary <- function (gr) {
  #group<<-subset(read.csv(paste(cd,"groups.csv", sep="/")),group==gr)
  group<<-subset(groupGames,group==gr)
  ### Load in team names by group/team id
  #master<<-subset(read.csv(paste(cd,"master.csv",sep="/")),group==gr,select=c(team,teamname))
  

  # gothru(groupA)
  
  # To get result for 2 outcomes: sapply(list(groupA,groupA2),gothru)
  

  
  ### Simulate given all scores with between 0 and 5 goals. 
  #group<-group
  simulation<-expand.grid(g1=c(0:5),g2=c(0:5),g3=c(0:5),g4=c(0:5))
  names(simulation)<-as.character(paste("g",subset(group,is.na(goals))$country,sep=""))
  results<-apply(simulation,1,simulate)
  simulation$first<-as.factor(results[1,])
  simulation$second<-as.factor(results[2,])
  
  #masterfirst<-master
  #names(masterfirst)<-c("first","firstname")
  #mastersecond<-master
  #names(mastersecond)<-c("second","secondname")
  
  simulation$score1<-sign(simulation[,1]-simulation[,2])
  simulation$score2<-sign(simulation[,3]-simulation[,4])
  
  #simulation<-merge(merge(simulation,masterfirst),mastersecond)
  simul<<-simulation

  
  ### Summary, aggregated by result (W/D/L) in each remaining game
  ## firstmd and secondmd are the team that comes first or second most often,
  ## and firstonly and secondonly are indicators for whether that team
  ## is always in first or second place (could not be because of goal difference, etc)
  summary<-ddply(simulation,c("score1","score2"),summarize,firstmd=Mode(first),firstonly=only(first),secondmd=Mode(second),secondonly=only(second))
  
  ### Making it look nice... 
  summary$score1[summary$score1==-1]<-"L"
  summary$score1[summary$score1==0]<-"T"
  summary$score1[summary$score1==1]<-"W"
  summary$score2[summary$score2==-1]<-"L"
  summary$score2[summary$score2==0]<-"T"
  summary$score2[summary$score2==1]<-"W"
  
  remaininggames<-subset(group,is.na(goals))
  summary$score1<-paste(remaininggames$country[1],
                        summary$score1,
                        remaininggames$country[2],
                        sep=" ")
  summary$score2<-paste(remaininggames$country[3],
                        summary$score2,
                        remaininggames$country[4],
                        sep=" ")
  
  #masterfirst<-master
  #names(masterfirst)<-c("firstmd","firstmdname")
  #mastersecond<-master
  #names(mastersecond)<-c("secondmd","secondmdname")
  
  #summary$group<-gr
  #summary<-merge(merge(summary,masterfirst),mastersecond)
  
  #summary$res1<-paste()
  return(summary)
}

### Possible outcomes given a certain score (for different goal combinations)
possible <- function (outcomes) {
  possoutcomes<-subset(simul,score1==outcomes[1] & score2==outcomes[2],select=-c(score1,score2))
  possoutcomes<-possoutcomes[order(possoutcomes$first,possoutcomes$second),]
  return(possoutcomes)
}

### Examples:
groupGames<-buildgroups(3)
#getsummary("A")
getsummary("G")
possible(c(1,1))

