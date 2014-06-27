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
groupGames<-buildgroups(1)
  
  output$grouprnk <- renderTable({    
    buildrankings(subset(groupGames, group==input$gr))
  })
})