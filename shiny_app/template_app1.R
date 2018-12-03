
library(shiny)
library(shinyTime)
library(timeDate)
library(leaflet)

df <- read.csv("data/complete_data.csv",encoding="UTF-8") 


ui <- fluidPage(
    mainPanel(
      
      tabsetPanel(type='tabs',
      
      tabPanel("time overview ",sidebarPanel("TITLE",
                               #selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu"),
                               #selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue"),
                               #sliderInput( "year_period", "THE SLIDE YEAR SHOW", min = 1900, max = 2017, value =c(1900,2017), step = 1) ,
                               radioButtons("time", "WHICH TIME ARE U ON ?", choices=c("by day in the year"="visit_date",
                                                                                             "by_day_in_the_week"="day_of_the_week_visit",
                                                                                             "by hour"="visit_time",
                                                                                             "by holidays"="holiday_flg_visit")
                                            ),
                               conditionalPanel(condition = "input.time =='visit_date'",
                                                dateRangeInput("daterange", "Date range:",
                                                               start = as.Date('2016-01-01'),
                                                               end = as.Date('2017-05-31')
                                                               )
                                                 ),
                               
                               conditionalPanel(condition = "input.time =='day_of_the_week_visit'",
                                                checkboxGroupInput("day", "Select something else", choices = c('Monday',
                                                                                                            'Tuesday',
                                                                                                            'Wednesday',
                                                                                                            'Thursday',
                                                                                                            'Friday',
                                                                                                            'Saturday',
                                                                                                            'Sunday'),
                                                                                                   selected=c('Monday',
                                                                                                              'Tuesday',
                                                                                                              'Wednesday',
                                                                                                              'Thursday',
                                                                                                              'Friday',
                                                                                                              'Saturday',
                                                                                                              'Sunday'))),
                               conditionalPanel(condition = "input.time =='visit_time'",
                                                numericInput("hours1","between ",value=0, min =0, max =23, step = 1),
                                                numericInput("hours2"," and ",value=23, min =0, max =23, step = 1)
                                                ),
                               
                               actionButton("refresh",label = "Refresh")
                               
                               ),
                        mainPanel(
                               plotOutput("plot_time"),
                               textOutput("number_visitors_period")
                              # DT::DTOutput("Table")
                               )
                        
               ),
      
      tabPanel("per restaurant",sidebarPanel("TITLE",
                                     selectInput("id","Choose your favorite restaurant ",choices=unique(df$id)),
                                     radioButtons("time2", "WHICH TIME ARE U ON ?", choices=c("by day in the year"="visit_date",
                                                                                             "by_day_in_the_week"="day_of_the_week_visit",
                                                                                             "by hour"="visit_time",
                                                                                             "by holidays"="holiday_flg_visit")
                                     ),
                                     conditionalPanel(condition = "input.time2 =='visit_date'",
                                                      dateRangeInput("daterange2", "Date range:",
                                                                     start = as.Date('2016-01-01'),
                                                                     end = as.Date('2017-05-31')
                                                      )
                                     ),
                                     
                                     conditionalPanel(condition = "input.time2 =='day_of_the_week_visit'",
                                                      checkboxGroupInput("day2", "Select something else", choices = c('Monday',
                                                                                                                     'Tuesday',
                                                                                                                     'Wednesday',
                                                                                                                     'Thursday',
                                                                                                                     'Friday',
                                                                                                                     'Saturday',
                                                                                                                     'Sunday'),
                                                                         selected=c('Monday',
                                                                                    'Tuesday',
                                                                                    'Wednesday',
                                                                                    'Thursday',
                                                                                    'Friday',
                                                                                    'Saturday',
                                                                                    'Sunday'))),
                                     conditionalPanel(condition = "input.time2 =='visit_time'",
                                                      numericInput("hours12","between ",value=0, min =0, max =23, step = 1),
                                                      numericInput("hours22"," and ",value=23, min =0, max =23, step = 1)
                                     ),
                                     
                                     actionButton("refresh2",label = "Refresh bis")
                                    ),
                 mainPanel(
                 plotOutput("plot_time2"),
                 textOutput("number_visitors_period2")
                 # DT::DTOutput("Table")
               )
              ),
      tabPanel("Map",
               mainPanel(
                 h5("Description of the map :\n\n"),
                 leafletOutput("map")
                 
                        )
        
        
              )
      
      
    
      )
    )
  )


server <- function(input, output) {
  

  library(tidyverse)
  library(RColorBrewer)
  

  
  
  count <- function (df,time){
    quo_time <- rlang::sym(time)
    
    if(time=='visit_date'){
      df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
      df <- df %>% 
        filter(!!quo_time >= input$daterange[1] & !!quo_time <= input$daterange[2]) %>%  
        mutate(visit_date=as.factor(visit_date))
    }
    if(time=='visit_time'){
      df <- df %>% 
        mutate(visit_time=as.character(visit_time)) %>%
        mutate( count=substr(visit_time, start = 1, stop = 3)) %>% 
        mutate(count=as.numeric(count)) %>% 
        filter(count >= input$hours1 & count <= input$hours2) %>% 
        mutate(visit_time=as.factor(visit_time)) %>% 
        select(-count)
      
    }
    if(time=='day_of_the_week_visit'){
      df <- df %>% 
        filter(day_of_the_week_visit %in% input$day)
    }
    df %>%
      filter(!is.na(!!quo_time)) %>% 
      filter(!is.na(visitors)) %>% 
      summarise(n = sum(visitors)) %>% 
      ungroup %>% 
      pull(n)
  }
  
  

  general_overview <- function(df,time){ 
    quo_time <- rlang::sym(time)
    str_time <- quo_name(quo_time)
    
    
    if(time=='visit_date'){
      df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
      df <- df %>% 
        filter(!!quo_time >= input$daterange[1] & !!quo_time <= input$daterange[2]) %>%  
        mutate(visit_date=as.factor(visit_date))
    }
    if(time=='visit_time'){
        df <- df %>% 
          mutate(visit_time=as.character(visit_time)) %>%
          mutate( count=substr(visit_time, start = 1, stop = 3)) %>% 
          mutate(count=as.numeric(count)) %>% 
          filter(count >= input$hours1 & count <= input$hours2) %>% 
          mutate(visit_time=as.factor(visit_time)) %>% 
          select(-count)
        
    }
    if(time=='day_of_the_week_visit'){
      df <- df %>% 
        filter(day_of_the_week_visit %in% input$day)
    }
    
    df %>%
      filter(!is.na(!!quo_time)) %>% 
      filter(!is.na(visitors)) %>% 
      group_by(!!quo_time) %>%
      summarise(total=sum(visitors)) %>% 
      
      ggplot(aes_(x =quo_time, y = as.name("total"),fill=quo_time))+
      geom_bar(stat='identity')+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "none")
    }
  
  personal_overview <- function(df,time){ 
    df <- df %>%  filter(id == input$id)
    general_overview(df,time)
  }
  
  personal_count <- function (df,time){
    df <- df %>%  filter(id == input$id)
    count(df,time)
  }
  
   pp1 <- eventReactive(input$refresh,{general_overview(df,input$time)})
   pp2 <- eventReactive(input$refresh,{count(df,input$time)})
   pp12 <- eventReactive(input$refresh2,{personal_overview(df,input$time2)})
   pp22 <- eventReactive(input$refresh2,{personal_count(df,input$time2)})
   
   output$plot_time <- renderPlot({
     pp1()
     })
   
   output$number_visitors_period <- renderText({ 
     glue::glue("They are {pp2()} visitors represented overall on this graph ")
     })
   
   output$plot_time2 <- renderPlot({
     pp12()
   })
   
   output$number_visitors_period2 <- renderText({ 
     glue::glue("They are {pp22()} visitors represented overall on this graph ")
   })
   
   output$map <- renderLeaflet({ 
     leaflet(df %>% distinct(id, .keep_all = TRUE)) %>%
       addTiles() %>%
       addProviderTiles("CartoDB.Positron") %>%
       addMarkers(~longitude, ~latitude,
                  popup = ~id, label = ~genre_name,
                  clusterOptions = markerClusterOptions())
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

