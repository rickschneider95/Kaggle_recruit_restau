
library(shiny)
library(RColorBrewer)


  
ui <- fluidPage(
    mainPanel(
      
      tabsetPanel(type='tabs',
      
      tabPanel("TEST 1",sidebarPanel("TITLE",
                               #selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu"),
                               selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue"),
                               #sliderInput( "year_period", "THE SLIDE YEAR SHOW", min = 1900, max = 2017, value =c(1900,2017), step = 1) ,
                               radioButtons("time", "WHICH TIME ARE U ON ?", choices=c("by day in the year"="visit_date",
                                                                                             "by day in the week"="day_of_the_week_visit",
                                                                                             "by hour"="visit_time",
                                                                                             "by holidays"="holiday_flg_visit"))#,
                               #actionButton("refresh",label = "Refresh")
                               
                               ),
                        mainPanel(
                               plotOutput("plot_time")#,
                               #textOutput("Total"),
                              # DT::DTOutput("Table")
                               )
                        
               ),
      
      tabPanel("TEST 2",sidebarPanel("TITLE",
                                     selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue")
                                    )
              )
      
      
    
      )
    )
  )


server <- function(input, output) {
  

library(tidyverse)
  df <- read.csv("data/complete_data.csv",encoding="UTF-8")  

  general_overview <- function(df,time){
      df %>%
      group_by(time) %>%
      summarise(total=sum(visitors)) %>%
      ggplot()+
      aes(x =time, y = total) +
      geom_line(col=input$color)
  }
  
  
     
   output$plot_time <- renderPlot({general_overview(df,input$time)
     })
   #output$Total <- renderText({
     #glue::glue("They were {count_a_name(input$name,dataset())} births of {input$name}")
     #})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

