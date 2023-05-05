library(shiny)
library(shinydashboard)
library(flexdashboard)
library(dplyr)

#updateSelectizeInput(session, inputId="searchbar", choices=c("test", "test2"), server=F)

shinyServer(
  function(input, output, session) {
    output$scoregauge <- flexdashboard::renderGauge({
      gauge(30, min = 0, max = 100, symbol = '%', label = paste("Critic score"), gaugeSectors(
        success = c(100, 75), warning = c(74,40), danger = c(39, 0))
      )
    })
  }
)
