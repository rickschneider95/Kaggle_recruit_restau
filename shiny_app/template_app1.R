
library(shiny)
library(shinyTime)
library(timeDate)
library(leaflet)
library(tidyverse)
library(RColorBrewer)


prediction <-read.csv("data/sub.csv",encoding="UTF-8") 
prediction <- prediction %>%
    separate(id, into = c('id','bin', 'visit_date'), sep = c(20,21)) %>% 
    select(-bin) %>% 
    mutate(visit_date=as.Date(visit_date))


df <- read.csv("data/air_data.csv",encoding="UTF-8")
df <- df %>% 
  mutate( visit_month =  case_when(is.na(visit_time) ~ visit_date)) %>% 
  mutate(visit_month= as.factor(months(as.Date(visit_month))))


###################################

# Sort factor levels arbitrarily
sortLvls.fnc <- function(oldFactor, levelOrder) {
  if(!is.factor(oldFactor)) stop("The variable you want to reorder isn't a factor.")
  if(!is.numeric(levelOrder)) stop("'order' should be a numeric vector.")
  if(max(levelOrder) > length(levels(oldFactor))) stop("The largest number in 'order' can't be larger than the number of levels in the factor.")
  if(length(levelOrder) > length(levels(oldFactor))) stop("You can't have more elements in 'order' than there are levels in the factor.")
  if(length(levelOrder) == length(levels(oldFactor))) {
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrder])
  }
  if(length(levelOrder) < length(levels(oldFactor))) {
    levelOrderAll <- c(levelOrder, (1:length(levels(oldFactor)))[-levelOrder])
    reorderedFactor <- factor(oldFactor, levels = levels(oldFactor)[levelOrderAll])
  }
  return(reorderedFactor)
}

df$day_of_the_week_visit <- sortLvls.fnc(df$day_of_the_week_visit, c(2, 6, 7, 5, 1,3,4))
df$visit_month <- sortLvls.fnc(df$visit_month, c(5,4,9,2,8,7,6,1,12,11,10,3))

###########################################################



ui <- fluidPage(
    mainPanel(
      
      tabsetPanel(type='tabs',
      
      tabPanel("Intro",mainPanel( width = 12,
        h3("Welcome to our shiny app", align = "center"),
        h4("we have 5 tabs on top  of the intro. They are"),
        h4(" 1/ Time overview "),
        h4(" 2/per restaurant"),
        h4(" 3/ map "),
        h4(" 4/by region/genre"),
        h4(" 5/ prediction"),
        h4(""),
        h4("In this app, we give the user an in-depth analysis of the frequentation of restaurants 
           in Japan. Our app can give more or less specific information, depending on the preferences of the user."),
        h4("First, the time overview tab:"),
        h4("Here, we don't differentiate between different restaurants. The user can enter different dates, be it months,
           days of week, or any partition of the year. The user can choose what time period to choose, and we display 
           the frequentation numbers for all restaurants in japan. Here of course the y-axis is very large numbers,
           as it is the number of visitors in all restaurants for a given 'time property'."),
        h4(""),
        h4("Second, the per restaurant tab:"),
        h4("Here, the user can specify the restaurant. This is more useful for the individual of course, although less so 
            for those doing large-scale studies.So the user can specify a specific restaurant by id (more on how to get
           that in the next tab) and then run the exact same functions as in tab 1, just now only for the restaurant
           corresponding to that id."),
        h4(""),
        h4("Third, the map tab:"),
        h4("Here we give an interactive map of japan. The user can zoom in and find the location of all the restaurants.
           For each restaurant, we can find its id and its genre (type of restaurant) as well as its location of course.
           This can be used in addition to the first two tabs to get a better idea of what the ids mean."),
        h4(""),
        h4("Fourth, the by region/genre:"),
        h4("Here, we can get visitors info but instead of by restaurant, we get them by the region and/or genre of the
           restaurant. What this means is we can specify the genre or region, and get visitation info based on that.
           Ie the user can for example want to know about cocktail bars in tokyo, perhaps for a market study or personal
           interest, and isolate the data relevant only to that specific subset.Of course, the x-axis here remains the
           same as it has always been, namely the subset of dates fitting the required constraints set by the user,
           but the numbers change to reflect the change of the dataset we are analysing."),
        h4(""),
        h4("Fifth, but not least, the prediction:"),
        h4("The dataset we were given contained only data up to april 22nd inclusive. However, using machine learning 
            prediction algorithms, we were able to predict the visitor data up to may 31st. This tab is that predicted data.
            Although it cannot be said to be truthful in the barest sense of the word, it is a good prediction of what can
            be expected. The data in this tab is forecasting future visits.It is presented in a similar way as the first 
            tab, but with reduced capabilities due to the small timeframe of the data in question. Namely:The user can pick
            an id and a date, and we output the predicted number of visitors for that restaurant on that day.")
        
        
        
      )
        
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
      tabPanel("Prediction",
               sidebarPanel("Title",
                            selectizeInput("id5",label = "Choose your favorite restaurant ",choices = unique(prediction$id), options = list(maxItems = 10)),
                            dateInput("date5","Choose you date:",value = as.Date('2017-04-23'),min=as.Date('2017-04-23'),max=as.Date('2017-05-31')),
                            actionButton("refresh5",label = "Refresh")
                            ),
               mainPanel(
                          DT::DTOutput("Table5")
                        )
               
              )
      
      
    
      )
    )
  )


server <- function(input, output) {
  

  

  

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
        filter(!!quo_time >= daterange[1] & !!quo_time <= daterange[2])  
        #mutate(visit_date=as.factor(visit_date))
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
  
  predictive_table <- function(df,ids,date){
    df %>%
      filter(id %in% ids) %>% 
      filter(visit_date %in% date)
  }
  
   pp1 <- eventReactive(input$refresh,{general_overview(df,input$time,input$daterange)})
   pp2 <- eventReactive(input$refresh,{count(df,input$time,input$daterange)})
   pp12 <- eventReactive(input$refresh2,{personal_overview(df,input$time2,input$daterange2)})
   pp22 <- eventReactive(input$refresh2,{personal_count(df,input$time2,input$daterange2)})
   pp14 <- eventReactive(input$refresh4,{area_genre_overview(df,input$time4,input$daterange4)})
   pp24 <- eventReactive(input$refresh4,{area_genre_count(df,input$time4,input$daterange4)})
   pp15 <- eventReactive(input$refresh5,{predictive_table(prediction,input$id5,input$date5)})
   
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
   output$Table5 <- DT::renderDT({pp15()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

