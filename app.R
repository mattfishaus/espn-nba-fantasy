library(shiny)

ui <- fluidPage(
  titlePanel("TP has rabies!!")
)

server <- function(input, output) {
  }

shinyApp(ui = ui, server = server)