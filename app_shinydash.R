library(tidyverse)
library(shiny)
library(shinyWidgets)
library(shinydashboard)  # Change to shinydashboard
library(DT)
library(here)

# Check directory
here::here()

source(here('code', 'Code.R'))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "JSA Results", titleWidth = 250),  # Dashboard header
  dashboardSidebar(
    sidebarMenu(
      menuItem("---- 1st Tier ----", 
               menuSubItem("Recommendation by Sport", tabName = "reco_sport"),
               menuSubItem("Recommendation by Athlete", tabName = "reco_athlete")),
      menuItem("---- 2nd Tier ----",
               menuSubItem("Recommendation by Sport", tabName = "reco_sport_2nd"),
               menuSubItem("Recommendation by Athlete", tabName = "reco_athlete_2nd")
      )
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "reco_sport",
        fluidRow(
          box(h3("Recommendation by Sport"), title = "Summary", width = 12, status = "primary")),
        fluidRow(
          column(width = 4,  # Adjust width of the column as needed
                 # Controls box
                 box(title = 'Controls',
                     selectInput(
                       inputId = 'sport',
                       label = 'Select Sport',
                       choices = names(match_data$match_by_sport),
                       selectize = TRUE,
                       width = '100%'
                     )
                 ),
                 # Criteria box below Controls
                 box(title = 'Criteria',
                     tableOutput('criteria')
                 )
          )),
        fluidRow(box(title = 'Results',
            DTOutput('match_bysport'), 
            width = 12)
        
      )
      ),
      tabItem(
        tabName = "reco_athlete",
        box(h1("Recommendation by Athlete"), title = "Summary", width = 12, status = "primary"),
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
          column(width = 8,
                 box(title = 'Results', DTOutput('match_byathlete'))
          )
        )
      ),
      tabItem(
        tabName = "reco_sport_2nd",
        fluidRow(
          box(h1("Recommendation by Sport"), title = "Summary", width = 12, status = "primary")
          ),
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
          fluidRow(
            box(title = 'Results', DTOutput('match_bysport_others'), width = 12)
          )
        )
      ),
      tabItem(
        tabName = "reco_athlete_2nd",
        box(h1("Recommendation by Athlete"), title = "Summary", width = 12, status = "primary"),
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
          column(width = 8,
                 box(title = 'Results', DTOutput('match_byathlete_others'))
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
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
      scrollY = TRUE,
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
}

shinyApp(ui, server)
