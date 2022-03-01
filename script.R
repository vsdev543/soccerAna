library(dplyr)
library(plotly)
library(lubridate)


callDat<-function(path="data/data.csv",dates=c('1998-08-30','2020-08-30')){
df<-as_tibble(read.csv(path))%>%
  mutate(date=ymd(date))

if(!is.null(dates)){
 df<-df%>%
   filter(date>=ymd(dates[1]),date<=ymd(dates[2]))
}


winner<-apply(df,1,function(row){
  if(row[["home_score"]]>row[["away_score"]]){
    row[["home_team"]]
  }else if(row[["home_score"]]<row[["away_score"]]){
    row[["away_team"]]
  }else{
    "Draw"
  }
})

df$winner<-winner

teams<-unique(c(unique(df$home_team),unique(df$away_team)))

homeScore<-df%>%
  group_by(team=home_team)%>%
  summarise(homeScore=sum(home_score))

awayScore<-df%>%
  group_by(team=away_team)%>%
  summarise(awayScore=sum(away_score))

wins<-df%>%
  group_by(team=winner)%>%
  summarise(matches.won=n())%>%
  filter(team!="Draw")

n.match<-sapply(teams,function(team){
  k<-df%>%
    filter(team==home_team | team==away_team)
  
  nrow(k)
})

n.match<-data.frame(team=names(n.match),total.matches=n.match)

out<- full_join(full_join(
  homeScore,awayScore,by="team"
),wins,by='team'
)%>%
  full_join(n.match,"team")

out$totalScore<-out$homeScore+out$awayScore

arrBy<-'totalScore'

out<-out%>%
  arrange(desc(out[,arrBy]))

m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)

figs<-apply(out,1,function(row){
    fig <- plot_ly(
      type = 'scatterpolar',
      r = row[-1],
      theta = names(row)[-1],
      fill = 'toself'
    )%>%
      layout(
        title=as.character(row[['team']]),
            polar = list(
              radialaxis = list(
                visible = T
              )
            ),
            showlegend = F,
        margin = m
      )
    
    fig
})

names(figs)<-out$team

saveRDS(out,"data/out.Rds")
saveRDS(figs,"data/figs.Rds")

}


# callDat("data/data.csv",dates=c('1998-08-30','2020-08-30'))
