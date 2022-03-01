library(shiny)
library(bs4Dash)
library(shinyWidgets)

library(openxlsx)
library(readxl)
library(dplyr)
library(DT)

library(plotly)

appName<-"soccerAna!"

source("modules/welcome.R")
source("modules/dash.R")
source("script.R")

dfN<-read.csv("data/data.csv")%>%
  mutate(date=ymd(date))

ui <- div(
  tags$head(
    tags$link(type='text/css',href="styles.css",rel="stylesheet"),
    tags$script(src = 'index.js'),
    HTML('<link rel="preconnect" href="https://fonts.googleapis.com">
          <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
          <link href="https://fonts.googleapis.com/css2?family=Poppins:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">')
  ),
  uiOutput("bodyMain")
)

server <- function(input, output, session) {
  r<-reactiveValues(k=0)
  
  output$bodyMain<-renderUI({
    if(r$k==0){
      welcome_UI(id='wel',title=appName)}else{
        dash_UI('dash',title=appName)
      }
  })
  
  callModule(welcome,id="wel")
  
  callModule(dash,id="dash")
  
  observeEvent(input[[NS(namespace = 'wel','goIn')]],{
    r$k<-r$k+1
  })
  
  observe({
    print(input[[
      NS(id = 'dRange',namespace = 'dash')
    ]])
    updateDateRangeInput(session = session,inputId = NS(id = 'dRange',namespace = 'dash'),start = min(dfN$date),end = max(dfN$date))
  })
  
}

shinyApp(ui, server)
