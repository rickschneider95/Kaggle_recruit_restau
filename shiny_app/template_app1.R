
library(shiny)
library(shinyTime)
library(timeDate)
library(leaflet)

df <- read.csv("data/air_data.csv",encoding="UTF-8") 
## if visit_time = NA pour ne pas avoir de doublon
## ordre des factors plot

df <- df %>% 
  mutate(visit_month= months(as.Date(visit_date)))


ui <- fluidPage(
    mainPanel(
      
      tabsetPanel(type='tabs',
      
      tabPanel("Intro",mainPanel()
        
               ),
      
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
                               plotOutput("plot_time",width = "180%",height = "600px"),
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
                 plotOutput("plot_time2",width = "180%",height = "600px"),
                 textOutput("number_visitors_period2")
                 # DT::DTOutput("Table")
               )
              ),
      tabPanel("Map",
               mainPanel(
                 h5("Description of the map :\n\n"),
                 leafletOutput("map")
                 
                        )
        
        
              ),
      tabPanel("Analyse by Genre/region",
               sidebarPanel("TITLE",radioButtons("time4", "WHICH TIME ARE U ON ?", choices=c("by day in the year"="visit_date",
                                                                                             "by month"="visit_month")
                                                ),
                                    conditionalPanel(condition = "input.time4 =='visit_date'",
                                             dateRangeInput("daterange4", "Date range:",
                                                            start = as.Date('2016-01-01'),
                                                            end = as.Date('2017-05-31')
                                                    )
                                                ),
                                    conditionalPanel(condition = "input.time4 =='visit_month'",
                                                 checkboxGroupInput("month4", 
                                                                    "Select month", 
                                                                    choices = unique(df$visit_month),
                                                                    selected=unique(df$visit_month)
                                                                    )
                                                 ),
                                    checkboxGroupInput("Area4","select area",choices=unique(df$area_name)),
                                    checkboxGroupInput("Genre4","select genre",choices=unique(df$genre_name)),
                                    actionButton("refresh4",label = "Refresh bis")
                 
                          ),
               mainPanel(
                         plotOutput("plot_time4",width = "180%",height = "600px"),
                         textOutput("number_visitors_period4")
                        )
        
              ),
      tabPanel("Prediction",mainPanel()
               
              )
      
      
    
      )
    )
  )


server <- function(input, output) {
  

  library(tidyverse)
  library(RColorBrewer)
  

  
  
  count <- function (df,time,daterange){
    quo_time <- rlang::sym(time)
    
    if(time=='visit_date'){
      df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
      df <- df %>% 
        filter(!!quo_time >= daterange[1] & !!quo_time <= daterange[2]) %>%  
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
  
  

  general_overview <- function(df,time,daterange){ 
    quo_time <- rlang::sym(time)
    str_time <- quo_name(quo_time)
    
    
    if(time=='visit_date'){
      df$visit_date =  as.Date(df$visit_date, format = "%Y-%m-%d")
      df <- df %>% 
        filter(!!quo_time >= daterange[1] & !!quo_time <= daterange[2]) %>%  
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
  
  personal_overview <- function(df,time,daterange){ 
    df <- df %>%  filter(id == input$id)
    general_overview(df,time,daterange)
  }
  
  personal_count <- function (df,time,daterange){
    df <- df %>%  filter(id == input$id)
    count(df,time,daterange)
  }
  area_genre_overview <- function(df,time,daterange){
    df <- df %>%
      filter(genre_name %in% input$Genre4) %>% 
      filter(area_name %in% input$Area4)
    
    if(time=='visit_month'){
      df <- df %>% 
        filter(visit_month %in% input$month4)
    }
    
    general_overview(df,time,daterange)
      
  }
  area_genre_count <-function(df,time,daterange){
    df <- df %>%
      filter(genre_name %in% input$Genre4) %>% 
      filter(area_name %in% input$Area4)
    
    if(time=='visit_month'){
      df <- df %>% 
        filter(visit_month %in% input$month4)
    }
      count(df,time,daterange)
  }
  
   pp1 <- eventReactive(input$refresh,{general_overview(df,input$time,input$daterange)})
   pp2 <- eventReactive(input$refresh,{count(df,input$time,input$daterange)})
   pp12 <- eventReactive(input$refresh2,{personal_overview(df,input$time2,input$daterange2)})
   pp22 <- eventReactive(input$refresh2,{personal_count(df,input$time2,input$daterange2)})
   pp14 <- eventReactive(input$refresh4,{area_genre_overview(df,input$time4,input$daterange4)})
   pp24 <- eventReactive(input$refresh4,{area_genre_count(df,input$time4,input$daterange4)})
   
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
   
   output$plot_time4 <- renderPlot({
     pp14()
   })
   
   output$number_visitors_period4 <- renderText({ 
     glue::glue("They are {pp24()} visitors represented overall on this graph ")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

