if (!require(shiny)){
  install.packages('shiny')
}
library(shiny)

# Define UI
ui <- fluidPage(
  
  # App title ----
  titlePanel("Shiny Test App"),
  
  # Input: Select a file
  fileInput("file", "Choose CSV File",
            accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
  
  # Output: Show contents of file
  tableOutput("contents")
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to read selected file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Show contents of file
  output$contents <- renderTable({
    data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
