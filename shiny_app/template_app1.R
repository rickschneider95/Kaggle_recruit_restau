
library(shiny)
library(RColorBrewer)

# Define UI for application that draws a histogram

  
ui <- fluidPage(
    
    titlePanel("Application Title"),
    
    navlistPanel(
      "Header A",
      tabPanel("Component 1"),
    
      tabPanel("Component 2"),
      
      "Header B",
      tabPanel("Component 3"),
      tabPanel("Component 4"),
      "-----",
      tabPanel("Component 5")
    )
  )


server <- function(input, output) {

  
  dataset <- eventReactive(input$refresh,{
    
    prenoms_france %>%
      filter(name == input$name, sex %in% input$sex) %>%
      group_by(year, name) %>%
      summarise(total = sum(n)) %>%
      ungroup() %>%
      complete(year = 1900:2017, name, fill = list(total = 0))
    
  })
  
  
  library(prenoms)
  library(tidyverse)
  data(prenoms)
  
  draw_a_name <- function(nom,data) {
   data %>% 
      # replace_na(list(total = 0)) %>%
      ggplot() +
      aes(x = year, y = total) +
      geom_line(col=input$color)+
      coord_cartesian(xlim=input$year_period)
  }
  
  count_a_name <- function(nom,data){
    data %>% filter(year %in% input$year_period[1]:input$year_period[2]) %>% 
      summarise(n = sum(total)) %>% ungroup %>% 
      pull(n)
  }
  
  table_a_name <- function(nom,data){
      data %>%
      filter(year %in% input$year_period[1]:input$year_period[2]) %>% 
      group_by(year) %>%
      summarise(total=sum(total))
    }
  
  
  
     
   output$plot_popularity <- renderPlot({draw_a_name(input$name,dataset())
     })
   output$Total <- renderText({
     glue::glue("They were {count_a_name(input$name,dataset())} births of {input$name}")
     })
   output$Table <- DT::renderDT({table_a_name(input$name,dataset())
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

