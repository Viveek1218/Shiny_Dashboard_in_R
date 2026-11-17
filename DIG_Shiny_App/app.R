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
      tabsetPanel(
        tabPanel(
          "Patient Characteristics",
      h3("Patient Characteristics"),
      plotOutput("histogram"),
      
      h3("Age Comparison by Treatment Group"),
      plotOutput("ageBoxplot")
       ),
       tabPanel(
        "Treatment Comparison",
        h3("Treatment Group Comparison"),
        p("Compare patient characteristics across treatment groups."),
        plotOutput("treatmentPlot")
      )
      )
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
  output$ageBoxplot <- renderPlot({
    boxplot(
      AGE ~ TRTMT,
      data = dig.df,
      col = c("skyblue", "lightgreen"),
      main = "Age Comparison by Treatment Group",
      xlab = "Treatment Group",
      ylab = "Age (years)"
    )
  }) 
  # Treatment Comparison Graph
  output$treatmentPlot <- renderPlot({
    
    boxplot(
      BMI ~ TRTMT,
      data = dig.df,
      col = c("skyblue", "lightgreen"),
      main = "BMI Comparison by Treatment Group",
      xlab = "Treatment Group",
      ylab = "Body Mass Index (BMI)"
    )
    
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)