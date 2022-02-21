library(dplyr)
library(plotly)
library(lubridate)

df<-as_tibble(read.csv("data/data.csv"))%>%
  rowwise()%>%
  mutate(date=ymd(date))

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

figs<-apply(out,1,function(row){
    fig <- plot_ly(
      type = 'scatterpolar',
      r = row[-1],
      theta = names(row)[-1],
      fill = 'toself'
    )%>%
      layout(
        title=as.character(row[['team']])
      )
    
    fig
})

out
saveRDS(figs,"data/figs.Rds")



