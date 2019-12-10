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

funWINSUM <- function(x) {
  if (x > 0) {
    return(1)
  } else {
    return(0)
  }
}

funTIESUM <- function(x) {
  if (x == 0) {
    return(1)
  } else {
    return(0)
  }
}

funLOSSSUM <- function(x) {
  if (x < 0) {
    return(1)
  } else {
    return(0)
  }
}

#create team names
teamNUM <- c(1,2,3,4,5,6,7,8,9,10,11,12)
teamNames <- c("DEE", "TREN", "FISH", "LTM", "COWC", "MANC", "ADAM", "OLI", "BKUN", "DANI", "STRA", "TOLE") 
teams <- data.frame(team=teamNUM, Name=teamNames)

#create opponent names
opponentNUM <- c(1,2,3,4,5,6,7,8,9,10,11,12)
opponentNames <- c("DEE", "TREN", "FISH", "LTM", "COWC", "MANC", "ADAM", "OLI", "BKUN", "DANI", "STRA", "TOLE") 
opponents <- data.frame(opponent=opponentNUM, Name=opponentNames)

#create new matchup_results data frame
results <- left_join(teams, matchup_results %>% 
  select(team, opponent, weekno, fgpct, ftpct, tpm, pts, reb, ast, stl, blk, tover) %>%
  group_by(weekno, team, opponent) %>%
  summarise(WIN = funWIN(fgpct) + funWIN(ftpct) + funWIN(tpm) + funWIN(pts) + funWIN(reb) + funWIN(ast) + funWIN(stl) + funWIN(blk) + funWIN(tover),
            TIE = funTIE(fgpct) + funTIE(ftpct) + funTIE(tpm) + funTIE(pts) + funTIE(reb) + funTIE(ast) + funTIE(stl) + funTIE(blk) + funTIE(tover),
            LOSS = funLOSS(fgpct) + funLOSS(ftpct) + funLOSS(tpm) + funLOSS(pts) + funLOSS(reb) + funLOSS(ast) + funLOSS(stl) + funLOSS(blk) + funLOSS(tover),
            RESULT = funRESULT(sum(fgpct) + sum(ftpct) + sum(tpm) + sum(pts) + sum(reb) + sum(ast) + sum(stl) + sum(blk) + sum(tover))),
  by = c('team'))

resultsSUM <- matchup_results %>% 
  select(weekno, team, opponent, fgpct, ftpct, tpm, pts, reb, ast, stl, blk, tover) %>%
  group_by(weekno, team, opponent) %>%  
  summarise(WINS = funWINSUM(sum(fgpct) + sum(ftpct) + sum(tpm) + sum(pts) + sum(reb) + sum(ast) + sum(stl) + sum(blk) + sum(tover)),
            TIES = funTIESUM(sum(fgpct) + sum(ftpct) + sum(tpm) + sum(pts) + sum(reb) + sum(ast) + sum(stl) + sum(blk) + sum(tover)),
            LOSSES = funLOSSSUM(sum(fgpct) + sum(ftpct) + sum(tpm) + sum(pts) + sum(reb) + sum(ast) + sum(stl) + sum(blk) + sum(tover)))

#create standings
standings <- left_join(teams, resultsSUM %>% group_by(team) %>% summarise(WINS = sum(WINS), TIES = sum(TIES), LOSSES = sum(LOSSES)), by = c('team'))

#create shinyapp
ui <- fluidPage(
  h2("The League - Standings"),
  fluidRow(
    selectInput("team", "Team:",
                c("All",
                  unique(as.character(standings$team))))
  ),
  # Create a new row for the table.
  DT::dataTableOutput("resultStandings"),
  
  h2("The League - Matchup Results"),
  fluidRow(
           selectInput("week", "Week:",
                       c("All",
                         unique(as.character(results$weekno)))),
           selectInput("team2", "Team:",
                       c("All",
                         unique(as.character(results$team))))
  ),
  # Create a new row for the table.
  DT::dataTableOutput("resultMatchups")
)

server <- function(input, output) {
  # Filter data based on selections
  output$resultStandings <- DT::renderDataTable(
    DT::datatable({
      data <- standings
      if (input$team != "All") {
        data <- data[data$team == input$team,]
      }
      data
    },options = list(autoWidth = FALSE, columnDefs = list(list(width = '50px', targets = "_all")), pageLength = 12)))
  
    output$resultMatchups <- DT::renderDataTable(
    DT::datatable({
    data <- results
    if (input$week != "All") {
      data <- data[data$weekno == input$week,]
    }
    if (input$team2 != "All") {
      data <- data[data$team == input$team2,]
    }
    data
  },options = list(autoWidth = FALSE, columnDefs = list(list(width = '50px', targets = "_all")), pageLength = 100)))
}

shinyApp(ui = ui, server = server)
