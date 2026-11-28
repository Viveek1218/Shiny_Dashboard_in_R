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
    selectInput("sex_input", "Sex:", choices = c("All", "Male", "Female"))
    
    ),
  
  
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
                                       ),
                           
                           selectInput("base_group", "Group By:",
                                       choices = c("TRTMT", "SEX", "HYPERTEN"),
                                       )
                           ),
                       
                       box(title = "Boxplot Comparison", width = 8,
                           plotOutput("base_plot", height = "300px")
                           )
                       ),
              
              fluidRow(box(title = "Summary Statistics", width = 12,
                           tableOutput("base_table")
                           )
                       )
              
          ), # End Baselines Tab
    
      
    # Clinical Outcomes tab
    tabItem(tabName = "outcomes", h2("Clinical Outcomes by Treatment"),
            
            fluidRow(box(title = "Outcome Options", width = 4, 
                         selectInput("outcome_var", "Select Outcome:",
                                     choices = c(
                                       "Mortality (DEATH)" = "DEATH",
                                       "Hospitalization (HOSP)" = "HOSP",
                                       "Cardiovascular Disease (CVD)" = "CVD",
                                       "Worsening Heart Failure (WHF)" = "WHF"),
                                     ),
                         
                         selectInput("facet_var", "Split By Subgroup:",
                                     choices = c("None", "CVD", "HYPERTEN", "SEX"),
                                     )
                         ),
                     
                     box(title = "Outcome Proportions", width = 8,
                         plotOutput("outcome_plot", height = "320px")
                         )
                     ),
            
            fluidRow(box(title = "Outcome Counts", width = 12,
                         tableOutput("outcome_table"))
                     )
            
            ), # End Clinical Outcomes Tab
    
    
    # Survival Tab
    tabItem(tabName = "survival", h2("Cumulative Mortality Risk Over Time"),
            
            fluidRow(box(title = "Survival Settings", width = 4,
                         selectInput("surv_group", "Compare Curves By:",
                                     choices = c("Treatment Group" = "TRTMT",
                                                 "Cardiovascular Disease" = "CVD",
                                                 "Worsening Heart Failure" = "WHF")
                                     )
                         ),
                     
                     box(title = "Mortality Risk Curve", width = 8,
                         plotOutput("surv_plot", height = "320px")
                         )
                     ),
            
            fluidRow(box(title = "Yearly Mortality Risk Milestones", width = 12,
                         tableOutput("surv_table"))
                     )
            ) # End survival tab
    
    ) # End tab_Items
    
  ) # End dashboard_Body
  
) # End dashboard_Page

    
 
  



  
  

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
    theme(legend.position = "none")})


# Tab 2: Summary table
output$base_table <- renderTable({filtered_df() %>%
    group_by_at(input$base_group) %>%
    summarise(Count = n(),
              Mean = round(mean(get(input$base_var), na.rm = TRUE), 2),
              SD = round(sd(get(input$base_var), na.rm = TRUE), 2),
              Median = round(median(get(input$base_var), na.rm = TRUE), 2))
  })


# Tab 3: Outcome stacked bar plot
output$outcome_plot <- renderPlot({
  p <- ggplot(filtered_df(), aes_string(x = "TRTMT", fill = input$outcome_var)) +
    geom_bar(position = "fill", color = "black") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Treatment Group", y = "Percentage of Patients", fill = input$outcome_var)
  
  if (input$facet_var != "None") {
    p <- p + facet_wrap(input$facet_var)
  }
  
  p
})


# Tab 3: Outcome counts table
output$outcome_table <- renderTable({filtered_df() %>% 
    count(TRTMT, .data[[input$outcome_var]]) %>%
    rename(Patients = n)
})


}






#-------------------------------------------------------------------------------

# Run Shiny app
shinyApp(ui = ui, server = server)
