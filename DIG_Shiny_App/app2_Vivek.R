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
      menuItem("Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("Baselines", tabName = "baselines", icon = icon("chart-bar")),
      menuItem("Outcomes", tabName = "outcomes", icon = icon("heartbeat")),
      menuItem("Survival", tabName = "survival", icon = icon("clock")),
      menuItem("Blood Pressure", tabName = "bp", icon = icon("braille"))
    ),
    
    # Global filters
    checkboxGroupInput(
      "trtmt_input", "Treatment Group:",
      choices = c("Placebo", "Treatment"),
      selected = c("Placebo", "Treatment")
    ),
    
    sliderInput(
      "age_input", "Age Range:",
      min = 21, max = 90, value = c(21, 90)
    ),
    
    selectInput(
      "sex_input", "Sex:",
      choices = c("All", "Male", "Female"),
      selected = "All"
    )
  ),
   dashboardBody(
    tabItems(
      tabItem(tabName = "overview", h1("Study Overview")),
      tabItem(tabName = "baselines", h1("Baseline Characteristics")),
      tabItem(tabName = "outcomes", h1("Clinical Outcomes")),
      tabItem(tabName = "survival", h1("Survival Analysis")),
      tabItem(tabName = "bp", h1("Blood Pressure Relationship"))
    )
  )
)

#-------------------------------------------------------------------------------

# Creating server

server <- function(input, output) {
  
}

#-------------------------------------------------------------------------------

# Run Shiny app
shinyApp(ui = ui, server = server)
