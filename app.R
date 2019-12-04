library(RPostgres)
library(shiny)
library(DT)

#connect to Postgres db
con <- dbConnect(RPostgres::Postgres(),dbname = 'roster_scores', host = 'db-postgresql-sgp1-84573-do-user-1917838-0.db.ondigitalocean.com', port = 25060, user = 'doadmin', password = 'lzyhz8inuf02262q')

#query the db and save data frame
score_players <- dbGetQuery(con, "SELECT * from player_scores")

#create new data frame by team
score_team <- score_players %>% select(weekno, teamno, fgm, fga, ftm, fta, tpm, pts, reb, ast, stl, blk, tover) %>% group_by(weekno, teamno) %>% summarise(fgPct= round(sum(fgm)/sum(fga),4), ftPct= round(sum(ftm)/sum(fta),4), tpm= sum(tpm), pts= sum(pts), reb= sum(reb), ast= sum(ast), stl= sum(stl), blk= sum(blk), tover= sum(tover))

#create shinyapp
ui <- fluidPage(
  h2("The League"),
  fluidRow(
           selectInput("week", "Week:",
                       c("All",
                         unique(as.character(score_team$weekno)))),
           selectInput("team", "Team:",
                       c("All",
                         unique(as.character(score_team$teamno))))
  ),
  # Create a new row for the table.
  DT::dataTableOutput("result")
)

server <- function(input, output) {
  # Filter data based on selections
  output$result <- DT::renderDataTable(DT::datatable({
    data <- score_team
    if (input$week != "All") {
      data <- data[data$weekno == input$week,]
    }
    if (input$team != "All") {
      data <- data[data$teamno == input$team,]
    }
    data
  }))
}

shinyApp(ui = ui, server = server)