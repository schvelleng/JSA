#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(semantic.dashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(color = "blue", title = "Dashboard Demo", inverted = TRUE,
                  dropdownMenu(type = "notifications",
                               taskItem("Project progress...", 50.777, color = "red"))),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "main", "Main"),
      menuItem(tabName = "extra", "Extra")
    )
  ),
  dashboardBody(
    tabItems(
      selected = 1,
      tabItem(
        tabName = "main",
        box(h1("main"), title = "A b c", width = 16, color = "orange")
      ),
      tabItem(
        tabName = "extra",
        h1("extra")
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
})

shinyApp(ui, server)

# # Run the application 
shinyApp(ui = ui, server = server)
