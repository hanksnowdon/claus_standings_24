# app.R
library(shiny)
library(cfbfastR)
library(dplyr)
library(tibble)
library(stringr)
library(readr)
library(tidyr)

# If you have a CFBD_API_KEY, load it here if needed. For example:
# Sys.setenv(CFBD_API_KEY = Sys.getenv("CFB_SECRET"))

# Load your saved data
picksfinal <- readRDS("picksfinal.rds")
teamlist <- readRDS("teamlist.rds")

# Function to fetch the latest game info
update_results <- function() {
  livegames <- cfbd_game_info(2024, season_type = "postseason") %>%
    filter(home_division != "iii" &
             home_division != "ii" &
             !str_detect(notes, "(FCS|SWAC) Championship")) %>%
    select(game_id, start_date, completed, home_id, home_team, home_points, away_id, away_team, away_points, notes)
  
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
                               NA),
           correct = case_when(
             str_detect(notes, "CFP Semifinal") ~ ifelse(pick_id == winning_id, 3, 0),
             str_detect(notes, "CFP National Championship") ~ ifelse(pick_id == winning_id, 5, 0),
             TRUE ~ ifelse(pick_id == winning_id, 1, 0)
           )
    )
  
  standings <- results %>%
    group_by(Name) %>%
    summarise(points = sum(correct, na.rm = TRUE)) %>%
    arrange(desc(points))
  
  webpage_picks <- results %>%
    select( game,  Date, `Time (EST)`, Name, pick,  Favorite, fav_points, Underdog, und_points, Spread, pick_id, fav_id, und_id, winning_id, completed, correct) %>%
    left_join(teamlist, by = c("winning_id" = "team_id")) %>%
    mutate(winner = ifelse(winning_id == 0, "Push", pick.y)) %>%
    select(-pick.y) %>%
    rename(Game = game,  Pick = pick.x ,`Und. Score`= und_points ,   `Fav. Score` = fav_points,   Winner = winner)
  
  list(standings = standings, webpage_picks = webpage_picks)
}

# Reactive values to store the updated data
data_cache <- reactiveVal(update_results())

# Define UI
ui <- fluidPage(
  titlePanel("Bowl Pool Standings"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh Data")
    ),
    mainPanel(
      h3("Current Standings"),
      tableOutput("standings_table"),
      h3("Picks and Results"),
      tableOutput("picks_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$refresh, {
    data_cache(update_results())
  })
  
  output$standings_table <- renderTable({
    data_cache()$standings
  })
  
  output$picks_table <- renderTable({
    data_cache()$webpage_picks
  })
}

# Run the application
shinyApp(ui = ui, server = server)
