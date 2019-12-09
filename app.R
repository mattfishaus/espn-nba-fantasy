library(RPostgres)
library(shiny)
library(dplyr)
library(DT)

#connect to Postgres db
conn <- dbConnect(RPostgres::Postgres(),dbname = 'roster_scores', host = 'db-postgresql-sgp1-84573-do-user-1917838-0.db.ondigitalocean.com', port = 25060, user = 'doadmin', password = 'lzyhz8inuf02262q')

#query the db and save data frame
matchup_results <- dbGetQuery(conn, "SELECT * from matchup_results")

#create functions
funWIN <- function(x) {
  if (x == 1) {
    return(1)
  } else {
    return(0)
  }
}

funTIE <- function(x) {
  if (x == 0) {
    return(1)
  } else {
    return(0)
  }
}

funLOSS <- function(x) {
  if (x == -1) {
    return(1)
  } else {
    return(0)
  }
}

funRESULT <- function(x) {
  if (x > 0) {
    return("WON")
  } else if (x == 0) {
    return("TIE")
  } else {
    return ("LOSS")
  }
}

#create new matchup_results data frame
matchup_results <- matchup_results %>% 
  select(weekno, team, opponent, fgpct, ftpct, tpm, pts, reb, ast, stl, blk, tover) %>%
  group_by(weekno, team, opponent) %>%
  summarise(WIN = funWIN(fgpct) + funWIN(ftpct) + funWIN(tpm) + funWIN(pts) + funWIN(reb) + funWIN(ast) + funWIN(stl) + funWIN(blk) + funWIN(tover),
            TIE = funTIE(fgpct) + funTIE(ftpct) + funTIE(tpm) + funTIE(pts) + funTIE(reb) + funTIE(ast) + funTIE(stl) + funTIE(blk) + funTIE(tover),
            LOSS = funLOSS(fgpct) + funLOSS(ftpct) + funLOSS(tpm) + funLOSS(pts) + funLOSS(reb) + funLOSS(ast) + funLOSS(stl) + funLOSS(blk) + funLOSS(tover),
            RESULT = funRESULT(sum(fgpct) + sum(ftpct) + sum(tpm) + sum(pts) + sum(reb) + sum(ast) + sum(stl) + sum(blk) + sum(tover)))

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
