setwd("~/ipl_csv_male")
getwd()
files<-list.files()

library(proto)
library(gsubfn)
library(RSQLite)
library(sqldf)
library(ggplot2)
library(dplyr)

a<-read.csv( files[1],header=FALSE,col.names=paste("V",seq(11)))
#head(a,35)
a$match=1
a$matchc=files[1]
for(i in 2:816){
  t<-read.csv( files[i],header=FALSE,col.names=paste("V",seq(11)))
  t$match=i
  t$matchc=files[i]
  a=rbind(a,t)
}
#head(a,35)
m<-filter(a,V.1=='ball')
#head(m)
details<-filter(a,V.1=='info')
details<-details[c(1,2,3,12,13)]
#head(details,30)
season<-filter(details,V.2=='season')[c(3,4,5)]
#head(season)
colnames(m)= c("Ball" ,"Inning" ,"Over" ,"Country" ,"P1" ,"P2" ,"B" ,"R" ,"R1" ,"Out" ,"Outp", "match", "matchc")


m$Over=as.numeric(m$Over)
m$Ball2<-6*floor(m$Over)+10*(m$Over-floor(m$Over))
m$Ball3<-as.character(round(10*(m$Over-floor(m$Over))))
#tail(m,50)

#join season in column of m

m1=merge(m,season,"matchc")
colnames(m1)= c("matchc", "Ball" ,"Inning" ,"Over" ,"Country" ,"P1" ,"P2" ,"B" ,"R" ,"R1" ,"Out" ,"Outp", "match","Ball2","Ball3","season","match2")
#tail(m1)


#####Total # and % of match -team won by > 5 wicket######
win_wicket<-filter(details,V.2=="winner_wickets")[c(3,4)]
colnames(win_wicket)<-c("win_wicket","match")
wicket_more5<-sum(ifelse(win_wicket$win_wicket>= 5,1,0))
100*wicket_more5/816
wicket10<-sum(ifelse(win_wicket$win_wicket== 10,1,0))
100*wicket10/816
