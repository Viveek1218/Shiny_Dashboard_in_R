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
       ),
        tabPanel(
          "Clinical Outcomes",
          h3("Clinical Outcomes Analysis"),
          p("Explore mortality and hospitalisation outcomes in the DIG Trial."),
          plotOutput("outcomePlot")
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
  # Clinical Outcomes - Mortality Comparison
  output$outcomePlot <- renderPlot({
    
    death_data <- table(dig.df$TRTMT, dig.df$DEATH)
    
    death_percent <- prop.table(death_data, margin = 1) * 100
    
    barplot(
      t(death_percent),
      beside = TRUE,
      col = c("skyblue", "lightgreen"),
      main = "Mortality Percentage by Treatment Group",
      xlab = "Treatment Group",
      ylab = "Percentage of Patients (%)",
      legend.text = c("Death Status 0", "Death Status 1"),
      ylim = c(0, 100)
    )
    
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)