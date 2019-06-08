library(shiny)
library(r2d3)

ui <- fluidPage(
  d3Output("d3", height = "900px")
)


server <- function(input, output) {
  output$d3 <- renderD3({
    r2d3(data = jsonlite::read_json("test.json"), 
         d3_version = 4, script = "forcegraph.js")
  })
}

shinyApp(ui = ui, server = server)