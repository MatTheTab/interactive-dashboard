library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)

#updateSelectizeInput(session, inputId="searchbar", choices=c("test", "test2"), server=F)

shinyServer(
  function(input, output, session) {
    
    output$scoregauge <- renderPlotly({
      fig <- plot_ly(
        type = "indicator",
        mode = "gauge+number",
        value = 85,
        title = list(text = "Critic score", font = list(size = 22, color="#dddddd")),
        gauge = list(
          axis = list(
            range = list(0, 100),
            tickwidth = 1,
            tickcolor = "#cccccc",
            tickfont=list(size=13),
            dtick="10",
            ticklabelstep=1,
            tickangle=0
            ),
          bar = list(color = "white", thickness=0.2),
          threshold = list(
            value=85,
            thickness=1,
            line = list(width=3, color="white")),
          bgcolor = "#cccccc",
          borderwidth = 0,
          steps = list(
            list(range = c(0, 40), color = "#FF0000"),
            list(range = c(41, 79), color="#FFCC33"),
            list(range = c(80, 100), color = "#66CC33"))
          )
        )
      
      fig <- fig %>%
        layout(
          paper_bgcolor = "#2c323b",
          font = list(color = "#dddddd", family = "Arial", size=50),
          margin = list(r=50, t=70, b=50)
        )
      
      fig
    })
    
    
  }
)
