library(shiny)
library(tidyverse)
library(xtable)

#Connect to Postgres db
con <- dbConnect(RPostgres::Postgres(),dbname = 'roster_scores', host = 'db-postgresql-sgp1-84573-do-user-1917838-0.db.ondigitalocean.com', port = 25060, user = 'doadmin', password = 'lzyhz8inuf02262q')

#Query the db and save data frame
score_players <- dbGetQuery(con, "SELECT * from team_scores")

ui <- fluidPage(
  titlePanel("The League Power Rankings"),
  tableOutput("table")
)

server <- function(input, output) {
  output$table <- renderTable(score_players)
}

shinyApp(ui = ui, server = server)