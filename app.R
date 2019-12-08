library(RPostgres)
library(shiny)
library(dplyr)
library(DT)
library(foreach)
library(arsenal)

#connect to Postgres db
conn <- dbConnect(RPostgres::Postgres(),dbname = 'roster_scores', host = 'db-postgresql-sgp1-84573-do-user-1917838-0.db.ondigitalocean.com', port = 25060, user = 'doadmin', password = 'lzyhz8inuf02262q')

#query the db and save data frame
player_scores <- dbGetQuery(conn, "SELECT * from player_scores")

#create new team_scores data frame
team_scores <- player_scores %>% select(weekno, teamno, fgm, fga, ftm, fta, tpm, pts, reb, ast, stl, blk, tover) %>% group_by(weekno, teamno) %>% summarise(fgPct= round(sum(fgm)/sum(fga),4), ftPct= round(sum(ftm)/sum(fta),4), tpm= sum(tpm), pts= sum(pts), reb= sum(reb), ast= sum(ast), stl= sum(stl), blk= sum(blk), tover= sum(tover))

#write team_scores data frame into Postgres
dbWriteTable(conn,"team_scores",team_scores)


nrow <- nrow(team_scores)
noweek <- length(unique(team_scores$weekno))
noteam <- length(unique(team_scores$teamno))

vs <- function(x,y) {
  (x - y)
}

matchup_scores <- for (i in 1:noweek) {
  week <- filter(team_scores,weekno %in% c(i))
    for (j in 1:noteam){
      team <- filter(team_scores,teamno %in% c(j))
      for (k in 1:noteam){
        opponent <- filter(team,teamno %nin% c(k))
        col <- function(team_scores){
          for (k in 1:nrow(opponent))
        }
        #print(opponent$tpm)
        print(sprintf("week %d, team %d opponent %d tpm %d", i, j, k, opponent$tpm))
            }
        }
    }




#function
vs <- function(x,y) {
  (x - y)
}

table1 <- sapply(team_scores, vs)

#compare teams 1 and 3
within(team_scores, by="weekno",{
  fgPct <- fgPct.x - fgPct.y
  ftPct <- ftPct.x - ftPct.y
  tpm <- tpm.x - tpm.y
  pts <- pts.x - pts.y
  reb <- reb.x - reb.y
  ast <- ast.x - ast.y
  stl <- stl.x - stl.y
  blk <- blk.x - blk.y
  tover <- tover.x - tover.y
})[c("weekno","fgPct","ftPct","tpm","pts","reb","ast","stl","blk","tover")]


by(team_scores, 1:nrow(team_scores), )


for (i in 1:nrow(team_scores)-1){
  if(team_scores[i, 11] == team_scores[i + 1,10]){
    print("OK")
  }
}

  
#function
var_cat <- function(x, y) {
  (x - y)
}

matchup_scores_dis<-data.frame(purrr::map(1:nrow(team_scores),function(ind) { distance=sqrt(sum((team_scores[ind,]['x.x']-team_scores[ind,]['x.y'])^2,(team_scores[ind,]['y.x']-team_scores[ind,]['y.y'])^2)) }))

matchup_scores_diff<-data.frame(purrr::map(1:nrow(team_scores),function(ind) { difference=(team_scores[ind,][unique(team_scores$teamno)]-team_scores[ind,][unique(team_scores$teamno)])^2 }))
matchup_scores_diff

matchup_scores <- combn(rownames(team_scores), 2, simplify = FALSE)
names(matchup_scores) <- sapply(matchup_scores, paste, collapse = " - ")
sapply(matchup_scores, var_cat(team_scores$tpm, team_scores$tpm))

x = foreach(i = cursor(team_scores, by = ))


grid2 <- cbind(
  expand.grid(Opponent = unique(team_scores$teamno), Team = team_scores$teamno),
  data.frame(team_scores$weekno)
)
grid2



#make a df for each team
team_1 <- filter(team_scores,teamno %in% c("1"))
team_2 <- filter(team_scores,teamno %in% c("2"))
team_3 <- filter(team_scores,teamno %in% c("3"))
team_4 <- filter(team_scores,teamno %in% c("4"))
team_5 <- filter(team_scores,teamno %in% c("5"))
team_6 <- filter(team_scores,teamno %in% c("6"))
team_7 <- filter(team_scores,teamno %in% c("7"))
team_8 <- filter(team_scores,teamno %in% c("8"))
team_9 <- filter(team_scores,teamno %in% c("9"))
team_10 <- filter(team_scores,teamno %in% c("10"))
team_11 <- filter(team_scores,teamno %in% c("11"))
team_12 <- filter(team_scores,teamno %in% c("12"))

comparedf(team_1, team_2)

library(foreach)
x <- foreach(i=1:length(unique(team_scores$weekno)), .combine = 'cbind') %do% rnorm(4)
x



#compare teams 1 and 2
for (i in 1:(team_scores$teamno)) {
  within(merge(team_[i], team_[i])) 
}
  

matchup_1vs2 <- within(merge(team_1, team_2, by="weekno"),{
  team <- "1"
  opponent <- "2"
  fgPct <- fgPct.x - fgPct.y
  ftPct <- ftPct.x - ftPct.y
  tpm <- tpm.x - tpm.y
  pts <- pts.x - pts.y
  reb <- reb.x - reb.y
  ast <- ast.x - ast.y
  stl <- stl.x - stl.y
  blk <- blk.x - blk.y
  tover <- tover.x - tover.y
})[,c("weekno","team","opponent","fgPct","ftPct","tpm","pts","reb","ast","stl","blk","tover")]

#compare teams 1 and 3
matchup_1vs3 <- within(merge(team_1, team_3, by="weekno"),{
  team <- "1"
  opponent <- "3"
  fgPct <- fgPct.x - fgPct.y
  ftPct <- ftPct.x - ftPct.y
  tpm <- tpm.x - tpm.y
  pts <- pts.x - pts.y
  reb <- reb.x - reb.y
  ast <- ast.x - ast.y
  stl <- stl.x - stl.y
  blk <- blk.x - blk.y
  tover <- tover.x - tover.y
})[,c("weekno","team","opponent","fgPct","ftPct","tpm","pts","reb","ast","stl","blk","tover")]

#merge
matchup_scores <- rbind(matchup_1vs2, matchup_1vs3)
matchup_scores


#create shinyapp
ui <- fluidPage(
  h2("The League"),
  fluidRow(
           selectInput("week", "Week:",
                       c("All",
                         unique(as.character(team_scores$weekno)))),
           selectInput("team", "Team:",
                       c("All",
                         unique(as.character(team_scores$teamno))))
  ),
  # Create a new row for the table.
  DT::dataTableOutput("result")
)

server <- function(input, output) {
  # Filter data based on selections
  output$result <- DT::renderDataTable(DT::datatable({
    data <- team_scores
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

#compare teams 1 and 2
#matchup_1vs2 <- within(merge(team_1, team_2, by="weekno"),{
#  team <- "1"
#  opponent <- "2"
#  fgPct <- ifelse(fgPct.x > fgPct.y,1,ifelse(fgPct.x == fgPct.y,0,-1))
#  ftPct <- ifelse(ftPct.x > ftPct.y,1,ifelse(ftPct.x == ftPct.y,0,-1))
#  tpm <- ifelse(tpm.x > tpm.y,1,ifelse(tpm.x == tpm.y,0,-1))
#  pts <- ifelse(pts.x > pts.y,1,ifelse(pts.x == pts.y,0,-1))
#  reb <- ifelse(reb.x > reb.y,1,ifelse(reb.x == reb.y,0,-1))
#  ast <- ifelse(ast.x > ast.y,1,ifelse(ast.x == ast.y,0,-1))
#  stl <- ifelse(stl.x > stl.y,1,ifelse(stl.x == stl.y,0,-1))
#  blk <- ifelse(blk.x > blk.y,1,ifelse(blk.x == blk.y,0,-1))
#  tover <- ifelse(tover.x > tover.y,1,ifelse(tover.x == tover.y,0,-1))
#})[,c("weekno","team","opponent","fgPct","ftPct","tpm","pts","reb","ast","stl","blk","tover")]
