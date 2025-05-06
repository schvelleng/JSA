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

# Source the external R script (change path)
# check directory
library(here)
here::here()

source(here('code', 'Code.R'))


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "JSA Results", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem("---- 1st Tier ----", tabName = NULL),
      menuItem("Recommendation by Sport", tabName = "reco_sport"),
      menuItem("Recommendation by Athlete", tabName = "reco_athlete"),
      menuItem("---- 2nd Tier ----", tabName = NULL),  # Visual divider (not clickable)
      menuItem("Recommendation by Sport", tabName = "reco_sport_2nd"),
      menuItem("Recommendation by Athlete", tabName = "reco_athlete_2nd")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "reco_sport",
        box(h1("Recommendation by Sport"), title = "Summary", width = 16, color = "orange"),
        fluidRow(
          column(width = 4,
                 box(title = 'Controls',
                     selectInput(
                       inputId = 'sport',
                       label = 'Select Sport',
                       choices = names(match_data$match_by_sport),
                       selectize = TRUE,
                       width = '100%')
                     ),
                 box(title = 'Criteria',
                     tableOutput('criteria')
                     )
                 ),
          column(width = 12,
                 box(title = 'Results', DTOutput('match_bysport')
                 )
          )
        )
      ),
      tabItem(
        tabName = "reco_athlete",
         box(h1("Recommendation by Athlete"), title = "Summary", width = 16, color = "orange"),
        fluidRow(
          column(width = 4,
                 box(title = 'Controls',
                     selectizeInput(
                       inputId = 'athlete',
                       label = 'Select Athlete',
                       choices = NULL,
                       multiple = FALSE)
                 )
          ),
          column(width = 12,
                 box(title = 'Results', DTOutput('match_byathlete')
                 )
          )
        )
      ),
      tabItem(
        tabName = "reco_sport_2nd",
        box(h1("Recommendation by Sport"), title = "Summary", width = 16, color = "orange"),
        fluidRow(
          column(width = 4,
                 box(title = 'Controls',
                     selectInput(
                       inputId = 'sport_other',
                       label = 'Select Sport',
                       choices = names(match_data_others$match_by_sport),
                       selectize = TRUE,
                       width = '100%')
                 ),
                 box(title = 'Criteria',
                     tableOutput('criteria_others')
                 )
          ),
          column(width = 12,
                 box(title = 'Results', DTOutput('match_bysport_others')
                 )
          )
        )
      ),
      tabItem(
        tabName = "reco_athlete_2nd",
        box(h1("Recommendation by Athlete"), title = "Summary", width = 16, color = "orange"),
        fluidRow(
          column(width = 4,
                 box(title = 'Controls',
                     selectizeInput(
                       inputId = 'athlete_other',
                       label = 'Select Athlete',
                       choices = NULL,
                       multiple = FALSE)
                 )
          ),
          column(width = 12,
                 box(title = 'Results', DTOutput('match_byathlete_others')
                 )
          )
        )
      )
    )
  )
)

   
server <- shinyServer(function(input, output, session) {
  


  # FIRST RECOMMENDATION
  select_sport <- reactive({
    input$sport
  })
  
  sport_criteria_name <- reactive({
    req(select_sport())
    
    names(criteria[[select_sport()]]) %>% 
      gsub('percentile_', '',.)
  })
  
  criteria_table <- reactive({
    criteria_split[[select_sport()]] %>% 
      pivot_longer(!sport, names_to = 'criteria', values_to = 'values') %>% 
      select(-sport)
  })
  
  output$criteria <- renderTable({
    criteria_table()},
    align = "l", width = "100%", spacing = 'xs'
  )
  
  sport_results <- reactive({
    req(select_sport())
    
    displayed_df <- match_data$match_by_sport[[select_sport()]] %>% 
      merge(df_percentiles, by = 'id') %>% 
      select(id, gender, dob, age, everything())
    
    if (select_sport() != 'Other') {
      displayed_df <- displayed_df %>% 
        select(id, score, gender, dob, age, matches(paste(sport_criteria_name(), collapse = "|"))) %>% 
        arrange(desc(score))
    }
    
    displayed_df
  })
  
  output$match_bysport <- renderDT({
    datatable(sport_results(),options = list(
      width= '100%',
      scrollX = TRUE,
      rownames = FALSE,
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ))
  })
  
  updateSelectizeInput(session, 'athlete', choices = names(match_data$match_by_athlete), server = TRUE)

  select_athlete <- reactive({
    input$athlete
  })

  athlete_results <- reactive({
    req(select_athlete())

    match_data$match_by_athlete[[select_athlete()]]

  })

  output$match_byathlete <- renderDT({
    datatable(athlete_results(),options = list(
      width= '100%',
      scrollX = TRUE,
      rownames = FALSE,
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ))
  })
  
  # SECOND RECOMMENDATION
  select_sport_others <- reactive({
    input$sport_other
  })
  
  sport_criteria_name_other <- reactive({
    req(select_sport_others())
    
    names(criteria_others[[select_sport_others()]]) %>% 
      gsub('percentile_', '',.)
  })
  
  criteria_table_other <- reactive({
    criteria_split_others[[select_sport_others()]] %>% 
      pivot_longer(!sport, names_to = 'criteria', values_to = 'values') %>% 
      select(-sport)
  })
  
  output$criteria_others <- renderTable({
    criteria_table_other()},
    align = "l", width = "100%", spacing = 'xs'
  )
  
  sport_results_others <- reactive({
    req(select_sport_others())
    
    displayed_df_others <- match_data_others$match_by_sport[[select_sport_others()]] %>% 
      merge(df_percentiles, by = 'id') %>% 
      select(id, gender, dob, age, everything())
    
    if (select_sport_others() != 'Other') {
      displayed_df_others <- displayed_df_others %>% 
        select(id, score, gender, dob, age, matches(paste(sport_criteria_name(), collapse = "|"))) %>% 
        arrange(desc(score))
    }
    
    displayed_df_others
  })
  
  output$match_bysport_others <- renderDT({
    datatable(sport_results_others(),options = list(
      width= '100%',
      scrollX = TRUE,
      rownames = FALSE,
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ))
  })
  
  updateSelectizeInput(session, 'athlete_other', choices = names(match_data_others$match_by_athlete), server = TRUE)
  
  select_athlete_other <- reactive({
    input$athlete_other
  })
  
  athlete_results_other <- reactive({
    req(select_athlete_other())
    
    match_data_others$match_by_athlete[[select_athlete_other()]]
    
  })
  
  output$match_byathlete_others <- renderDT({
    datatable(athlete_results_other(),options = list(
      width= '100%',
      scrollX = TRUE,
      rownames = FALSE,
      columnDefs = list(list(className = 'dt-left', targets = "_all"))
    ))
  })
  
})

shinyApp(ui, server)

# # Run the application 
shinyApp(ui = ui, server = server)
