#-------------------------------------------------------------------------------
# Loading libraries

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)


# Loading and cleaning data
dig.df <- read_csv("data/DIG.csv")

dig.df <- dig.df %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, 
         SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
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
      # Overview Tab
      tabItem(tabName = "overview", h1("Study Overview"),
              
              fluidRow(valueBoxOutput("box_total", width = 4),
                       valueBoxOutput("box_death", width = 4),
                       valueBoxOutput("box_hosp", width = 4)
                       ),
              
              fluidRow(box(title = "About the Study",
                           status = "success",
                           solidHeader = TRUE,
                           width = 12,
                           p("The Digitalis Investigation Group (DIG) trial was a randomized, double-blind clinical trial examining the safety and efficacy of Digoxin in patients with heart failure."),
                           p("Patients with an ejection fraction of 45% or less were randomized to receive either placebo or digoxin. Key outcomes tracked include mortality, worsening heart failure, and hospitalizations."),
                           p("Note: To protect patient privacy, variables in this dataset were permuted within treatment groups. Use the filters on the left sidebar to explore different patient subgroups.")
                           )
                       ),
              
              fluidRow(box(title = "Patients by Treatment Group",
                           width = 6,
                           plotOutput("overview_plot", height = "250px")
                           ),
                       
                       box(title = "Treatment Group Counts",
                           width = 6,
                           tableOutput("overview_table")
                           )
                       )
              ), # End Overview Tab
      
      
      # Baseline Characteristics Tab
      tabItem(tabName = "baselines", h2("Baseline Characteristics"),
              
              fluidRow(box(title = "Select Variable", width = 4,
                           
                           selectInput("base_var", "Variable:",
                                       choices = c("AGE", "BMI", "SYSBP", "DIABP", "CREAT", "KLEVEL"),
                                       selected = "AGE"),
                           selectInput("base_group", "Group By:",
                                       choices = c("TRTMT", "SEX", "HYPERTEN"),
                                       selected = "TRTMT")
                           ),
                       
                       box(title = "Boxplot Comparison", width = 8,
                           plotOutput("base_plot", height = "300px")
                           )
                       ),
              
              fluidRow(box(title = "Summary Statistics", width = 12,
                           tableOutput("base_table")
                           )
                       )
              
              ) # End Baselines Tab
      
      ) # End tabItems 
    
    ) # End dashboardBody
  
) # End dashboardPage


  
  

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
  


  
# Value boxes for Overview tab

output$box_total <- renderValueBox({
  total_n <- nrow(filtered_df())
  valueBox(total_n, "Total Patients Selected", icon = icon("users"), color = "blue")
  })

output$box_death <- renderValueBox({
  df <- filtered_df()
  death_rate <- round(mean(df$DEATH == "Death", na.rm = TRUE) * 100, 1)
  valueBox(paste0(death_rate, "%"), "Mortality Rate", icon = icon("heart-broken"), color = "red")
})

output$box_hosp <- renderValueBox({
  df <- filtered_df()
  hosp_rate <- round(mean(df$HOSP == "Yes", na.rm = TRUE) * 100, 1)
  valueBox(paste0(hosp_rate, "%"), "Hospitalization Rate", icon = icon("hospital"), color = "orange")
})


# Bar plot for treatment groups (Tab 1)
output$overview_plot <- renderPlot({
  ggplot(filtered_df(), aes(x = TRTMT, fill = TRTMT)) +
    geom_bar() +
    scale_fill_manual(values = c("Placebo" = "orange", "Treatment" = "skyblue")) +
    labs(x = "Treatment Group", y = "Number of Patients") +
    theme_minimal() +
    theme(legend.position = "none")
})


# Summary table for treatment groups (Tab 1)
output$overview_table <- renderTable({
  filtered_df() %>%
    group_by(TRTMT) %>%
    summarise(Count = n(), Percentage = round((n() / nrow(filtered_df())) * 100, 1)) %>%
    rename("Treatment Group" = TRTMT)
})


# Tab 2: Boxplot
output$base_plot <- renderPlot({
  ggplot(filtered_df(), aes_string(x = input$base_group, y = input$base_var, fill = input$base_group)) +
    geom_boxplot() +
    labs(x = input$base_group, y = input$base_var) +
    theme_minimal() +
    theme(legend.position = "none")
})

# Tab 2: Summary table
output$base_table <- renderTable({
  filtered_df() %>%
    group_by_at(input$base_group) %>%
    summarise(
      Count = n(),
      Mean = round(mean(get(input$base_var), na.rm = TRUE), 2),
      SD = round(sd(get(input$base_var), na.rm = TRUE), 2),
      Median = round(median(get(input$base_var), na.rm = TRUE), 2)
    )
})


}






#-------------------------------------------------------------------------------

# Run Shiny app
shinyApp(ui = ui, server = server)
