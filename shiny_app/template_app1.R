
library(shiny)
library(RColorBrewer)

# Define UI for application that draws a histogram

  
ui <- fluidPage(
    mainPanel(
      
      tabsetPanel(type='tabs',
      
      tabPanel("TEST 1",sidebarPanel("TITLE",
                               selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu"),
                               selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue"),
                               sliderInput( "year_period", "THE SLIDE YEAR SHOW", min = 1900, max = 2017, value =c(1900,2017), step = 1) ,
                               checkboxGroupInput("sex", "WHICH SEX ARE U ON ?", choices=c("Boyss"="M","Girlss"="F")),
                               actionButton("refresh",label = "Refresh")
                               
                               ),
                        mainPanel(
                               plotOutput("plot_popularity"),
                               textOutput("Total"),
                               DT::DTOutput("Table")
                               )
                        
               ),
      
      tabPanel("TEST 2",sidebarPanel("TITLE",
                                     selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu")
                                    )
              )
      
      
    
      )
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

