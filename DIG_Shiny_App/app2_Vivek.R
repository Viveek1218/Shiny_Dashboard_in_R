#-------------------------------------------------------------------------------
# Loading libraries

library(shiny)
library(shinydashboard)
library(tidyverse)


# Loading and cleaning data
dig.df <- read_csv("data/DIG.csv")

dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
  mutate(ID = as.character(ID),
         
         TRTMT = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment")),
         SEX = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
         DEATH = factor(DEATH, levels = c(0, 1), labels = c("Alive", "Death")),
         
         HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No", "Yes")),
         CVD = factor(CVD, levels = c(0, 1), labels = c("No", "Yes")),
         WHF = factor(WHF, levels = c(0, 1), labels = c("No", "Yes")),
         DIG = factor(DIG, levels = c(0, 1), labels = c("No", "Yes")),
         HOSP = factor(HOSP, levels = c(0, 1), labels = c("No", "Yes")),
         
         Month = round(DEATHDAY / 30)
         )

#-------------------------------------------------------------------------------

# Creating dashboard interface
  
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "DIG Trial Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("info")),
      menuItem("Baselines", tabName = "baselines", icon = icon("line-chart")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("heartbeat")),
      menuItem("Survival", tabName = "survival", icon = icon("clock")),
      menuItem("Blood Pressure", tabName = "bp", icon = icon("stethoscope"))
    ),
    
    # Adding Global filters that will work on entire dashboard
    checkboxGroupInput("trtmt_input", "Treatment Group:",
                       choices = c("Placebo", "Treatment"),
                       selected = c("Placebo", "Treatment")),
    
    sliderInput("age_input", "Age Range:", min = 21, max = 90, value = c(21, 90)),
    selectInput("sex_input", "Sex:", choices = c("All", "Male", "Female"), selected = "All")),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview", h2("DIG Trial Overview"), 
              fluidRow(box(title = "About the Study", status = "success", solidHeader = TRUE,      # "success" keeps the green color of theme set
                    p("The Digitalis Investigation Group (DIG) trial was a randomized, double-blind clinical trial examining the safety and efficacy of Digoxin in patients with heart failure."),
                    p("Patients with an ejection fraction of 45% or less were randomized to receive either placebo or digoxin. Key outcomes tracked include mortality, worsening heart failure, and hospitalizations."),
                    p("Note: To protect patient privacy, variables in this dataset were permuted within treatment groups. Use the filters on the left sidebar to explore different patient subgroups."))
                    )
              )
      )
    )
  )

#-------------------------------------------------------------------------------

# Creating server

server <- function(input, output) {
  
  filtered_df <- reactive({
    
    dig.df %>%
      filter(TRTMT %in% input$trtmt_input,
             AGE >= input$age_input[1],
             AGE <= input$age_input[2],
             input$sex_input == "All" | SEX == input$sex_input)
  })
  
}

#-------------------------------------------------------------------------------

# Run Shiny app
shinyApp(ui = ui, server = server)
