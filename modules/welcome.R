welcome_UI <- function(id,title) {
  ns <- NS(id)
  div(style="height:100vh; width:100vw; position:relative;overflow-x:hidden;overflow-y:hidden;background-image: url('Vector 1.svg');  background-repeat: no-repeat;
  background-size: cover;",
    
      img(src="play.png",style="width:500px; position:absolute; left:4vw; top:20vh;",class='fade-it'),
      
    div(style="position:absolute; right:2vw; top:15vh; text-align:right; width: 50vw;",
        span("WELCOME TO",style="font-weight:900; font-size:3vw;color:#1E91D6;"),br(),
        span(class="brand",title,style="font-weight:900; font-size:7vw;"),hr(),br(),
        actionBttn(inputId = ns('goIn'),"Proceed",style = "jelly",size = 'lg'),
        br(),br(),br(),br(),
        div(style="text-align:left;margin-left:8vw;font-weight:800;background-color:#333; padding:20px; border-radius:10px",
        "De dataset is afkomstig van Kaggle. De dataset bevat data van internationale voetbalwedstrijden(verschillende internationale toernooien en vriendschappelijke interlands) van 1872 tot en met 2021. Er zal worden onderzocht wie de best presterende landen waren.",br(),
        "Dit zal vanaf 4 perspectieven worden bekeken:",
        tags$ul(
          tags$li("Gewonnen wedstrijden"),
          tags$li("Thuis goals"),
          tags$li("Uit goals"),
         tags$li("Totale goals") 
        ),
        tags$a("Data source",href= "https://www.kaggle.com/martj42/international-football-results-from-1872-to-2017",style="color:#1E91D6;")
        ),
        
        
        # span("The goal of this resource is to simplify the development of pesticide detection methods for all vendor instruments.",
        #      style="font-weight:800; font-size:1vw;"
        #      ),br(),
        # span("All data contained in this resource has been curated from the literature or developed personally by Ben Orsburn as part of the www.LCMSmethods.org initiative.",
        #      style="font-weight:800; font-size:1vw;"
        # ),br(),
        # span("No guarantees of any kind are made or should be implied.",
        #      style="font-weight:800; font-size:1vw;"
        # ),br(),
        # span("For further details please see the upcoming publication on oPestTL; Orsburn, 2022.",
        #      style="font-weight:800; font-size:1vw;"
        # ),
        br()
        
        )
  )
}

welcome <- function(input, output, session) {

}