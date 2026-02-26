library(tidyverse)
library(janitor)
library(shiny)
library(shinydashboard)

elephants <- read_csv("data/elephants_data/elephants.csv") %>%
  clean_names()

ui <- dashboardPage(
  
  dashboardHeader(title="Age and Height of elephants by sex"),
  
  dashboardSidebar(
    
    selectInput("x",
                "Select Variable",
                choices = c("age","height"),
                selected = "age")),
  
  dashboardBody(
    plotOutput("plot", width ="600px", height ="500px"))
  
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    elephants %>% 
      ggplot(aes(x=sex, y= .data[[input$x]], fill=sex))+
      geom_boxplot()+
      theme_classic()
  })
}

shinyApp(ui, server)