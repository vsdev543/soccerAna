

m <- list(
  l = 50,
  r = 50,
  b = 50,
  t = 50,
  pad = 20
)

dash_UI <- function(id,title) {
  ns <- NS(id)
  fluidPage(style="padding:0px 5vw; background-color: #364156; color:#fff; height:100vh; overflow-y: auto;",
      div(style="display:flex; justify-content:space-between;align-items:baseline;",
        span(title,style="font-size:2.5em;margin-top:10px; font-weight:900;",class="brand"),
        radioGroupButtons(
          inputId = ns("tab"),
          label = NULL,
          choices = c("Stat", 
                      "Ranking"),
          status = "success"
        )
        ),
      fluidRow(column(9,
                          uiOutput(ns('main'))
                      ),
               column(3, 
                      dateRangeInput(inputId = ns('dRange'),label = NULL,start = NULL,end = NULL,width = "100%"),
                      actionBttn(ns("calR"),"Calculate",icon = icon("refresh"),style = 'stretch',block = T),hr(),
                      uiOutput(ns('side'))
               ))
    
  )
}

dash <- function(input, output, session) {
  ns<-session$ns
  
  df.r<-reactiveFileReader(intervalMillis = 1000,session = session,filePath = "data/out.Rds",readFunc = readRDS)
  
  observeEvent(input$calR,{
    withProgress(message = "Calculating",
    callDat("data/data.csv",dates=input$dRange)
    )
  })
  
  df<-reactive({
    df<-df.r()
    
    if(input$sortBy!=""){
    if(!is.null(input$sortBy)){
      if(input$sortBy=="team"){
      df<-df%>%
        arrange(df[,input$sortBy])
      }else{
        df<-df%>%
          arrange(desc(df[,input$sortBy]))
      }
    }
    }
    df
  })
  
  dff<-reactive({
    req(input[[paste0('homeScore',"W")]])
    df<-df.r()%>%
      select(team,homeScore, awayScore, matches.won, total.matches)%>%
      mutate(
        homeScore=input[[paste0('homeScore',"W")]]*homeScore,
        awayScore=input[[paste0('awayScore',"W")]]*awayScore,
        matches.won=input[[paste0('matches.won',"W")]]*matches.won,
        total.matches=input[[paste0('total.matches',"W")]]*total.matches
        )%>%
      mutate(score=homeScore+awayScore+matches.won+total.matches)%>%
      arrange(desc(score))
    
    df<-df[1:input$topY,]
    df$idx<-1:nrow(df)
    
    df
  })
  
  
  
  
  figs<-reactiveFileReader(intervalMillis = 1000,session = session,filePath = "data/figs.Rds",readFunc = function(path){
    figs<-readRDS(path)
    names(figs)<-df.r()$team 
    figs
  })
  
  output$side<-renderUI({
    switch(input$tab,
           "Stat" = {
             div(class="leftT",
               style = 'background-color:#212D40;border-radius:10px;padding:10px;',
               selectInput(
                 ns("sortBy"),
                 "Sort by",
                 choices = names(df.r()),
                 width = "100%"
               ),
               selectInput(
                 ns("topX"),
                 "Number of top entries",
                 choices = c(10, 20, 50, 100, nrow(df.r())),
                 width = "100%"
               ),
               selectInput(
                 ns("param"),
                 "Parameters",
                 choices = c("All", names(df.r())[-1]),
                 width = "100%"
               )
             )
           },
           "Ranking" = {
             div(class="leftT",
               style = 'background-color:#212D40;border-radius:10px;padding:10px;',
               lapply(c('homeScore', 'awayScore', 'matches.won', 'total.matches'),function(param){
                 sliderInput(inputId = ns(paste0(param,"W")),label = paste("weight for", param),min = 0,max = 1,value = 1,width = "100%")
               }),
               selectInput(
                 ns("topY"),
                 "Number of top entries",
                 choices = c(10, 20, 50, 100, nrow(df.r())),
                 width = "100%"
               )
             )
           }
           )
  })
  
  
  output$main<-renderUI({
    switch(input$tab,
           "Stat" = {
             switch (input$param,
               "All" = {
                 div(class = 'grid-2',
                     style = "width=100vw;",
                     lapply(df()$team[1:input$topX], function(t) {
                       renderPlotly(figs()[[t]])
                     }))
               },
               "homeScore" = {
                 df<-df()[1:input$topX,]%>%
                   select(c("team",input$param))
                 
                 plot_ly(df,type = 'bar',x=~team,y=~homeScore)%>%
                   layout(title=paste("homeScore of Top",input$topX,"teams by",input$sortBy),
                          margin = m
                          )
               },
               "awayScore" = {
                 df<-df()[1:input$topX,]%>%
                   select(c("team",input$param))
                 
                 plot_ly(df,type = 'bar',x=~team,y=~awayScore)%>%
                   layout(title=paste("awayScore of Top",input$topX,"teams by",input$sortBy),
                          margin = m
                   )
               },
               "matches.won" = {
                 df<-df()[1:input$topX,]%>%
                   select(c("team",input$param))
                 
                 plot_ly(df,type = 'bar',x=~team,y=~matches.won)%>%
                   layout(title=paste("matches.won of Top",input$topX,"teams by",input$sortBy),
                          margin = m
                   )
               },
               "totalScore" = {
                 df<-df()[1:input$topX,]%>%
                   select(c("team",awayScore,homeScore))
                 
                 plot_ly(df,type = 'bar',x=~team,y=~homeScore,name = 'homeScore')%>%
                   add_trace(data = df,x=~team,y=~awayScore,name="awayScore")%>%
                   layout(title=paste("totalScore of Top",input$topX,"teams by",input$sortBy),
                          margin = m,
                          barmode = 'stack',
                          yaxis = list(title = 'Score')
                   )
               },
               'total.matches' = {
                 df<-df()[1:input$topX,]%>%
                   select(c("team",total.matches,matches.won))%>%
                   mutate(matches.lost=total.matches-matches.won)
                 
                 plot_ly(df,type = 'bar',x=~team,y=~matches.lost,name = 'matches.lost')%>%
                   add_trace(data = df,x=~team,y=~matches.won,name="matches.won")%>%
                   layout(title=paste("total.matches of Top",input$topX,"teams by",input$sortBy),
                          margin = m,
                          barmode = 'stack',
                          yaxis = list(title = 'Matches')
                   )
               }
             )
           },
           "Ranking" = {
             tagList(
             lapply(dff()$team,function(t){
               row<-df()[df()$team==t,]
               div(style="background-color:#fff; padding:10px; border-radius:10px; margin-bottom:20px;display:flex;",class="leftT",
                plotlyOutput(ns(paste(t,"pl")),width = '15vw',height = '200px'),
                # div(style='width:600px;',
                #   progressBar(value = row[["homeScore"]],total = row[["totalScore"]],status = 'success',id = paste(t,'pr'))
                # ),
                plotlyOutput(ns(paste(t,"plB")),width = '35vw',height = '200px'),
                div(class="leftT",
                  style="width:200px; height:200px; border-radius:50%; border:solid 20px #DA4167; display:flex; justify-content:center; align-items:center; color:#333; font-weight:900; font-size:2em;text-align:center;",
                  "#",dff()[dff()$team==t,"idx"],br(),
                  dff()[dff()$team==t,"score"]
                )
               )
             })
             
             )
           }
           )
  })
  
  observe({
    req(dff())
    lapply(dff()$team,function(t){
      row<-df()[df()$team==t,]
      
        output[[paste(t,"pl")]]<-renderPlotly(figs()[[t]])
        
        output[[paste(t,"plB")]]<-renderPlotly({
          plot_ly(y = c("total.matches","totalScore"), x = c(row$matches.won,row$homeScore),type = 'bar', orientation = 'h',
                  text = c("matches.won","homeScore")
                  )%>%
            add_trace(y = c("total.matches","totalScore"), x = c(row$total.matches-row$matches.won,row$awayScore),
                      text = c("matches.lost","awayScore")
                      )%>%
            layout(barmode = 'stack',showlegend = FALSE)
        })
    })
  })
  
}
