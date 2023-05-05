library(shiny)
library(shinydashboard)
library(dplyr)
library(ECharts2Shiny)

#updateSelectizeInput(session, inputId="searchbar", choices=c("test", "test2"), server=F)

shinyServer(
  function(input, output, session) {
    renderGauge(div_id = "test", rate=99, gauge_name = "FinishRate")

  }
)
