# app.R
library(shiny)
library(dplyr)
library(tibble)
# library(stringr)
library(readr)
library(tidyr)
library(googlesheets4)
library(purrr)
library(DT) # ensure DT is loaded here


# Load your saved data
picksfinal <- readRDS("picksfinal.rds")
teamlist <- readRDS("teamlist.rds")

gs4_deauth()

sheet_url <- "https://docs.google.com/spreadsheets/d/1AqCmTVOOt_Cr7oRm61tQ8690VBjfufa4BS-1igNqMCc/edit?usp=sharing"
 
# Function to fetch the latest game info
update_results <- function() {
  livegames <- read_sheet(sheet_url, sheet = "Games")  %>%
    mutate(
      home_points = map_dbl(home_points, ~ as.numeric(.x[[1]])),
      away_points = map_dbl(away_points, ~ as.numeric(.x[[1]]))
    )
  
  results <- picksfinal %>%
    inner_join(livegames, by = "game_id") %>%
    rename(pick_id= team_id.x,
           fav_id = team_id.y,
           und_id = team_id) %>%
    mutate(fav_points = ifelse(
      completed == TRUE & home_id == fav_id, home_points,
      ifelse(
        completed == TRUE & away_id == fav_id, away_points,
        NA
      )
    ),
    und_points = ifelse(
      completed == TRUE & home_id == und_id, home_points,
      ifelse(
        completed == TRUE & away_id == und_id, away_points,
        NA
      )
    )) %>%
    mutate(winning_id = ifelse(completed == TRUE,
                               case_when(fav_points - Spread > und_points ~ fav_id,
                                         fav_points - Spread < und_points ~ und_id,
                                         TRUE ~ fav_id),
                               NA)
           ,
           points = ifelse(
             grepl("2 points", game),
             ifelse(pick_id == winning_id, 2, 0),
             ifelse(
               grepl("4 points", game),
               ifelse(pick_id == winning_id, 4, 0),
               ifelse(
                 grepl("6 points", game),
                 ifelse(pick_id == winning_id, 6, 0),
                 ifelse(pick_id == winning_id, 1, 0)
               )
             )
           )
    )
  
  standings <- results %>%
    group_by(Name) %>%
    summarise(points = sum(points, na.rm = TRUE)) %>%
    arrange(desc(points))
  
  webpage_picks <- results %>%
    select( game,  Date, `Time (EST)`, Name, pick,  Favorite, fav_points, Underdog, und_points, Spread, pick_id, fav_id, und_id, winning_id, completed, points) %>%
    left_join(teamlist, by = c("winning_id" = "team_id")) %>%
    mutate(winner = ifelse(winning_id == 0, "Push", pick.y)) %>%
    select(-pick.y, -pick_id, -fav_id, -und_id, -winning_id) %>%
    rename(Game = game,  Pick = pick.x ,`Und. Score`= und_points ,   `Fav. Score` = fav_points,   Winner = winner)
  
  list(standings = standings, webpage_picks = webpage_picks)
}

# Reactive values to store the updated data
data_cache <- reactiveVal(update_results())

# Define UI
ui <- navbarPage("CLAuS Bowl 2024",
                 
                 # Standings Page
                 tabPanel("Standings",
                          fluidPage(
                            titlePanel("Current Standings"),
                            DTOutput("standings_table")
                          )
                 ),
                 
                 # Results/Picks Page
                 tabPanel("Results/Picks",
                          fluidPage(
                            titlePanel("Results and Picks"),
                            sidebarLayout(
                              sidebarPanel(
                                # Dropdown to select a person
                                selectInput("selected_person", "Select a Person:", choices = NULL) 
                              ),
                              mainPanel(
                                DTOutput("picks_table")
                              )
                            )
                          )
                 )
)

# Define server logic
server <- function(input, output, session) {
 
  # Update the person dropdown whenever data changes
  observe({
    req(data_cache()$webpage_picks)
    all_names <- sort(unique(data_cache()$webpage_picks$Name))
    updateSelectInput(session, "selected_person", choices = all_names, selected = all_names[1])
  })
  
  # Standings table
  output$standings_table <- renderDT({
    datatable(data_cache()$standings, options = list(
      dom = 't',          # only show table
      pageLength = 25,    # can still show 25 rows by default
      autoWidth = TRUE
    ))
  })
  
  # Filter picks by selected person
  filtered_picks <- reactive({
    req(input$selected_person)
    data_cache()$webpage_picks %>%
      filter(Name == input$selected_person)
  })
  
  # Results/Picks table with conditional formatting
  output$picks_table <- renderDT({
    req(filtered_picks())
    datatable(filtered_picks(), # Enable the extension
              extensions = 'FixedHeader',
              options = list(
                pageLength = 45,
                autoWidth = TRUE,
                scrollX = TRUE,
                scrollY = '500px',   # set desired table height for vertical scroll
                fixedHeader = TRUE,  # freeze the header upon scrolling
                dom = 't'            # hide search bar / show bar if desired
              ),
              rownames = FALSE) %>%
      formatStyle(
        # Assume 'points' is numeric (0 or 1, or could be >1 if semifinal)
        columns = c("Game", "Date", "Time (EST)", "Name", "Pick", "Favorite", "Fav. Score", "Underdog", "Und. Score", "Spread", "Winner", "points", "completed"),
        valueColumns = "points",
        backgroundColor = styleEqual(
          c(0, 1, 2, 4,6), # possible values: 0=incorrect, 1=correct normal bowl, 3=correct semifinal, 5=correct championship
          c("#ffe6e6", "#e6ffe2", "#e6ffe6", "#e6ffe6", "#e6ffe6") # green for correct picks (1,3,5), red for incorrect (0)
        )
      )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)



