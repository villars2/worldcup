library(shiny)
library(reshape)
library(reshape2)
library(plyr)
library(rjson)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Expression that generates a histogram. The expression is
  # wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should re-execute automatically
  #     when inputs change
  #  2) Its output type is a plot
  

  
  # groupA<-data.frame(game=c(1,1,2,2,3,3,4,4,5,5,6,6),team=c(1,2,3,4,1,3,2,4,1,4,2,3))
  # groupA$goals<-c(rpois(8,1),rep(NA,4))
  
  ### Set directory and load in scores 
  #cd<-"C:/Users/Sergio/Documents/GitHub/worldcup"
  #cd<-"C:/Users/sv2307/Documents/GitHub/worldcup"
  
  ##### Put functions here
  ### Pulling data from world cup API.
  ## Argument=1 is to pull file from API, 2 is to get file and save, 3 is to
  ## read file from filename (only body of name, no .csv)


  
  output$distPlot <- renderPlot({
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
    
    ### Function to break ties
    buildtiebreak <- function(rnk,grp) {
      group<-rnk
      group<-group[order(group$points,group$gd,group$gf,decreasing=TRUE),]
      group$dup<-duplicated(subset(group,select=-c(country)),fromLast=TRUE)
      check<-as.numeric(subset(group,dup==TRUE,select=c(points,gd,gf))[1,])
      group$dups<-apply(as.matrix(subset(group,selec=c(points,gd,gf))),1,function(x) {identical(as.numeric(x),check)})
      tied<-group$country[group$dups]
      games<-grp
      games$tie<-as.factor(games$country) %in% tied
      
      #agg<-aggregate(games,)
      
      #group$dups<-(subset(group,select=c(points,gd,gf))==as.numeric(subset(group,dup==TRUE,select=c(points,gd,gf))))
      agg<-aggregate(games$tie,list(games$game),min)
      names(agg)<-c("game","tiedgames")
      games<-merge(games,agg)
      tiedgroup<-subset(games,tiedgames==1,select=c(game,country,fifa_code,goals,group))
      
      tiedgroup$teamingame<-rep(c(1,2),length(tiedgroup[,1])/2)
      tiebreak<-subset(rnk,select=country)
      tiebreak$brk<-rep(1,4)
      
      if (length(tiedgroup[,1])>0) {
        tiebreak<-buildrankings(tiedgroup)
        tiebreak$brk<-(1:length(tiedgroup[,1]))
        tiebreak<-subset(tiebreak,select=c(country,brk))             
      }
      return(tiebreak)
    }
    
    ### Function to build group rankings
    buildrankings <- function (grp) {
      grp$teamingame<-rep(c(1,2),length(grp[,1])/2)
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
      return(rankings)
    }
    
    ### Function to get top 2 teams (in order) in each group given scores
    gothru<-function(grp) {
      #   grp$teamingame<-rep(c(1,2),6)
      #   grp.wide<-dcast(grp,game~teamingame,value.var="goals")
      #   names(grp.wide)<-c("game","g1","g2")
      #   grp.wide$gd1<-grp.wide$g1-grp.wide$g2
      #   grp.wide$gd2<-grp.wide$g2-grp.wide$g1
      #   grp.wide<-subset(grp.wide,select=c(game,gd1,gd2))
      #   grp.gd<-melt(grp.wide,id=c("game"))
      #   grp.gd$variable<-substring(grp.gd$variable,3,4)
      #   names(grp.gd)<-c("game","teamingame","gd")
      #   
      #   grp.all<-merge(grp,grp.gd)
      #   grp.all$pts<-sign(grp.all$gd)+1+as.numeric(grp.all$gd>0)
      #   
      #   rankings<-ddply(grp.all,"country",summarize,points=sum(pts,na.rm=TRUE),gd=sum(gd,na.rm=TRUE),gf=sum(goals,na.rm=TRUE))
      #   rankings<-rankings[order(rankings$points,rankings$gd,rankings$gf,decreasing=TRUE),]
      rankings<-buildrankings(grp)
      rankings<-merge(rankings,buildtiebreak(rankings,grp))
      rankings<-rankings[order(rankings$points,rankings$gd,rankings$gf,-rankings$brk,decreasing=TRUE),]
      thru<-rankings$country[1:2]
      ### When considering complete ties, rank using 
      ### order<-5-rank(paste(as.character(rnk$points),as.character(rnk$gd),as.character(rnk$gf),as.character(rnk$rnk)),ties.method="max")
      ### Then, if more than one 1, name all ones the same (ARGxBRAxFRA,etc)
      ### If more than one 2, name one as first, and name all 2s the same (ARGxBRA, etc)
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
      # Here we could automatically make the simulation grid using, but would get big (1.7E6) rows
      gamesleft<-nrow(subset(group,is.na(goals)))
      simulation<-expand.grid(as.data.frame(matrix(rep(0:5,gamesleft),ncol=gamesleft)))
      # simulation<-expand.grid(g1=c(0:5),g2=c(0:5),g3=c(0:5),g4=c(0:5))
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
    
    ### Plot outcomes given scores:
    plotpossible <- function(df) {
      df$score1<-df[,1]-df[,2]
      df$score2<-df[,3]-df[,4]
      g1<-paste(names(df)[1],names(df)[2],sep="-")
      g2<-paste(names(df)[3],names(df)[4],sep="-")
      df<-subset(df,select=c(first,second,score1,score2))
      df.l<<-melt(df,id=c("score1","score2"))
      names(df.l)<-c(g1,g2,"rank","country")  
      g<-ggplot(df.l,aes(x=jitter(df.l[,1],amount=0.1),y=jitter(df.l[,2],amount=0.1),colour=country))+
        geom_point(size=2,alpha=0.7)+
        facet_grid(.~rank)+
        xlab(g1)+
        ylab(g2)+
        theme_bw()
      return(g)
    }
    
    
    groupGames<-buildgroups(1)
    getsummary(input$gr)
    plotpossible(simul)
  })
})