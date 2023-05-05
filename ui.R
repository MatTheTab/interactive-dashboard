library(shiny)
library(shinydashboard)
library(DT)

prettyTable <- function(table_df, round_columns_func=is.numeric, round_digits=0) {
  DT::datatable(table_df, style="bootstrap", filter = "top", rownames = FALSE, extensions = "Buttons", 
                options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatRound(unlist(lapply(table_df, round_columns_func)), round_digits)
}

dashboardPage(
  dashboardHeader(title="Game recommendation system", titleWidth = 350),
  
  dashboardSidebar(width = 130,
                   sidebarMenu(
                     menuItem("Dashboard", tabName="dashboard", icon = icon("gamepad")),
                     menuItem("Help", tabName="help", icon = icon("circle-question", class="fa-solid")),
                     menuItem("About", tabName="about", icon = icon("circle-info"))
                   )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="dashboard",
             fluidRow(
               box(
                 title = "Box title", width = 6, status = "primary",
                 "Box content"
               ),
               box(
                 status = "warning", width = 6,
                 "Box content"
               )
             ),
             
             fluidRow(
               column(width = 4,
                      box(
                        title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
                        "Box content"
                      ),
                      box(
                        width = NULL, background = "black",
                        "A box with a solid black background"
                      )
               ),
               
               column(width = 4,
                      box(
                        title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
                        "Box content"
                      ),
                      box(
                        title = "Title 5", width = NULL, background = "light-blue",
                        "A box with a solid light-blue background"
                      )
               ),
               
               column(width = 4,
                      box(
                        title = "Title 2", width = NULL, solidHeader = TRUE,
                        "Box content"
                      ),
                      box(
                        title = "Title 6", width = NULL, background = "maroon",
                        "A box with a solid maroon background"
                      )
               )
             )
      
      ),
      tabItem(tabName="help",
             h2("Help section")
      ),
      tabItem(tabName="about",
             h2("About section")
      )
    )
  )
)