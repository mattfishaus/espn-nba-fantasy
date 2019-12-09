library(RPostgres)
library(shiny)
library(dplyr)
library(DT)

#connect to Postgres db
conn <- dbConnect(RPostgres::Postgres(),dbname = 'roster_scores', host = 'db-postgresql-sgp1-84573-do-user-1917838-0.db.ondigitalocean.com', port = 25060, user = 'doadmin', password = 'lzyhz8inuf02262q')

#query the db and save data frame
matchup_scores <- dbGetQuery(conn, "SELECT * from matchup_results")

#create functions
varCat <- function(x) {
  if (x > 0) {
    return(1)
  } else if (x == 0) {
    return(0)
  } else {
    return(-1)
  }
}

resultMatch <- function(x) {
  if (x > 0) {
    return("WIN")
  } else if (x == 0) {
    return("TIE")
  } else {
    return("LOSS")
  }
}

#create new matchup_results data frame
matchup_results <- matchup_results %>% 
  select(weekno, team, opponent, fgpct, ftpct, tpm, pts, reb, ast, stl, blk, tover) %>%
  group_by(weekno, team, opponent) %>% 
  summarise(WINS = matchup_results$fgpct + matchup_results$ftpct + matchup_results$tpm + matchup_results$pts + matchup_results$reb + matchup_results$ast =  matchup_results$stl + matchup_results$blk + matchup_results$tover)

#create shinyapp
ui <- fluidPage(
  h2("The League Power Rankings"),
  fluidRow(
           selectInput("week", "Week:",
                       c("All",
                         unique(as.character(matchup_results$weekno)))),
           selectInput("team", "Team:",
                       c("All",
                         unique(as.character(matchup_results$team))))
  ),
  # Create a new row for the table.
  DT::dataTableOutput("result")
)

server <- function(input, output) {
  # Filter data based on selections
  output$result <- DT::renderDataTable(
    DT::datatable({
    data <- matchup_results
    if (input$week != "All") {
      data <- data[data$weekno == input$week,]
    }
    if (input$team != "All") {
      data <- data[data$team == input$team,]
    }
    data
  },options = list(autoWidth = FALSE, columnDefs = list(list(width = '50px', targets = "_all")), pageLength = 11)))
}

shinyApp(ui = ui, server = server)
