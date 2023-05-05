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
        value = 80,
        title = list(text = "Critic score", font = list(size = 24, color="#dddddd")),
        gauge = list(
          axis = list(
            range = list(0, 100),
            tickwidth = 1,
            tickcolor = "#cccccc",
            tickfont=list(size=20),
            dtick="10",
            ticklabelstep=1,
            tickangle=0
            ),
          bar = list(color = "pink", thickness=0.4),
          bgcolor = "#cccccc",
          borderwidth = 0,
          steps = list(
            list(range = c(0, 40), color = "red"),
            list(range = c(41, 79), color="yellow"),
            list(range = c(80, 100), color = "green"))
          )
        )
      
      fig <- fig %>%
        layout(
          paper_bgcolor = "#2c323b",
          font = list(color = "#dddddd", family = "Arial"),
          margin = list(r=50)
        )
      
      fig
    })
  }
)
