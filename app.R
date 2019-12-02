library(shiny)

ui <- fluidPage(
  titlePanel("The League Power Rankings")
)

server <- function(input, output) {
  }

shinyApp(ui = ui, server = server)