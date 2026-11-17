library(shiny)

# Load DIG Trial dataset
dig.df <- read.csv("data/DIG.csv")

# Create dashboard interface
ui <- fluidPage(
  
  titlePanel("DIG Trial Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "variable",
        "Select Patient Characteristic:",
        choices = c("AGE", "BMI", "SYSBP"),
        selected = "AGE"
      )
    ),
    
    mainPanel(
      h3("Patient Characteristics"),
      plotOutput("histogram")
    )
  )
)

# Create server
server <- function(input, output) {
  
  output$histogram <- renderPlot({
    
    hist(
      dig.df[[input$variable]],
      col = "skyblue",
      border = "white",
      main = paste("Distribution of", input$variable),
      xlab = input$variable
    )
    
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)