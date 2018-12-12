
library(shiny)
library(shinyTime)
library(timeDate)
library(leaflet)
library(tidyverse)
library(RColorBrewer)


path_aird <- system.file("shiny_csv", "air_data_shiny.csv", package = "recruitjap")
path_sub <- system.file("shiny_csv", "sub_shiny.csv", package = "recruitjap")
path_test <- system.file("shiny_csv", "test_prediction.csv", package = "recruitjap")


prediction <-read.csv(path_sub,encoding="UTF-8")
prediction <- prediction %>%
    separate(id, into = c('id','bin', 'visit_date'), sep = c(20,21)) %>%
    select(-bin) %>%
    mutate(visit_date=as.Date(visit_date))

test.prediction <- read.csv(path_test,encoding="UTF-8")


df <- read.csv(path_aird,encoding="UTF-8")
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
        h3(strong("Welcome to our shiny app"), align = "center"),
        h4("we have 6 tabs on top  of the intro. They are"),
        h4(" 1/ Time overview "),
        h4(" 2/ Per restaurant"),
        h4(" 3/ Map "),
        h4(" 4/ By region/genre"),
        h4(" 5/ Prediction"),
        h4(" 6/ Error Evaluation"),
        h4(""),
        h4("In this app, we give the user an in-depth analysis of the frequentation of restaurants
           in Japan. Our app can give more or less specific information, depending on the preferences of the user."),
        h4(strong("First, the time overview tab:")),
        h4("Here, we don't differentiate between different restaurants. The user can enter different dates, be it months,
           days of week, or any partition of the year. The user can choose what time period to choose, and we display
           the frequentation numbers for all restaurants in japan. Here of course the y-axis is very large numbers,
           as it is the number of visitors in all restaurants for a given 'time property'."),
        h4(""),
        h4(strong("Second, the per restaurant tab:")),
        h4("Here, the user can specify the restaurant. This is more useful for the individual of course, although less so
            for those doing large-scale studies.So the user can specify a specific restaurant by id (more on how to get
           that in the next tab) and then run the exact same functions as in tab 1, just now only for the restaurant
           corresponding to that id."),
        h4(""),
        h4(strong("Third, the map tab:")),
        h4("Here we give an interactive map of japan. The user can zoom in and find the location of all the restaurants.
           For each restaurant, we can find its id and its genre (type of restaurant) as well as its location of course.
           This can be used in addition to the first two tabs to get a better idea of what the ids mean."),
        h4(""),
        h4(strong("Fourth, the by region/genre:")),
        h4("Here, we can get visitors info but instead of by restaurant, we get them by the region and/or genre of the
           restaurant. What this means is we can specify the genre or region, and get visitation info based on that.
           Ie the user can for example want to know about cocktail bars in tokyo, perhaps for a market study or personal
           interest, and isolate the data relevant only to that specific subset.Of course, the x-axis here remains the
           same as it has always been, namely the subset of dates fitting the required constraints set by the user,
           but the numbers change to reflect the change of the dataset we are analysing."),
        h4(""),
        h4(strong("Fifth, the prediction:")),
        h4("The dataset we were given contained only data up to april 22nd inclusive. However, using machine learning
            prediction algorithms, we were able to predict the visitor data up to may 31st. This tab is that predicted data.
            Although it cannot be said to be truthful in the barest sense of the word, it is a good prediction of what can
            be expected. The data in this tab is forecasting future visits.It is presented in a similar way as the first
            tab, but with reduced capabilities due to the small timeframe of the data in question. Namely:The user can pick
            an id and a date, and we output the predicted number of visitors for that restaurant on that day."),
        h4(strong("Sixth, the error evaluation:")),
        h4("This tab lets us choose n and then displays a graph, where each point is the difference between the actual number
            of visitors and the predicted number of visitors, on the test dataset on which we ran our prediction.
            We notice it is mostly centred at 0 , which makes sense. As n increases, we gain in information but also lose in
            aestheticism, so the user must be careful which to choose.The existence of prediction errors is unavoidable and should not
            be seen as an indication of a flawed model, although of course the goal is to minimise them.This tab allows the user less
            personalisation with the settings (s)he can choose, that is because its main intent is to show the power of our prediction,
            rather than be able to get information about specific types of restaurants, as was the case in previous tabs.")



      )

               ),

      tabPanel("Time overview ",sidebarPanel(strong("Output selection"),
                               #selectInput("name","CHOICES ARE",choices=unique(prenoms$name),selected="Mathieu"),
                               #selectInput("color","Choose your favorite color bitch",choices=colors(),selected = "blue"),
                               #sliderInput( "year_period", "THE SLIDE YEAR SHOW", min = 1900, max = 2017, value =c(1900,2017), step = 1) ,
                               radioButtons("time", "Select a time span :", choices=c("by day in the year"="visit_date",
                                                                                      "by day of the week"="day_of_the_week_visit",
                                                                                      "by hours"="visit_time",
                                                                                      "by holidays"="holiday_flg_visit")
                                            ),
                               conditionalPanel(condition = "input.time =='visit_date'",
                                                dateRangeInput("daterange", "Date range:",
                                                               start = as.Date('2016-01-01'),
                                                               end = as.Date('2017-05-31'),
                                                               min=as.Date('2016-01-01'),
                                                               max=as.Date('2017-05-31')
                                                               )
                                                 ),

                               conditionalPanel(condition = "input.time =='day_of_the_week_visit'",
                                                checkboxGroupInput("day", "Weekday selection: ", choices = c('Monday',
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
                                                h5(strong("select your timeslot : ")),
                                                numericInput("hours1","between ",value=0, min =0, max =23, step = 1),
                                                numericInput("hours2"," and ",value=23, min =0, max =23, step = 1),
                                                h5(strong("hours"))
                                                ),

                               actionButton("refresh",label = "Refresh")

                               ),
                        mainPanel(
                               plotOutput("plot_time",width = "180%",height = "600px"),
                               textOutput("number_visitors_period")
                              # DT::DTOutput("Table")
                               )

               ),

      tabPanel("Per restaurant",sidebarPanel(strong("Output selection"),
                                     selectInput("id","Choose a restaurant ID: ",choices=unique(df$id)),
                                     radioButtons("time2", "Select a time span :", choices=c("by day in the year"="visit_date",
                                                                                             "by day of the week"="day_of_the_week_visit",
                                                                                             "by hours"="visit_time",
                                                                                             "by holidays"="holiday_flg_visit")
                                     ),
                                     conditionalPanel(condition = "input.time2 =='visit_date'",
                                                      dateRangeInput("daterange2", "Date range:",
                                                                     start = as.Date('2016-01-01'),
                                                                     end = as.Date('2017-05-31'),
                                                                     min=as.Date('2016-01-01'),
                                                                     max=as.Date('2017-05-31')
                                                      )
                                     ),

                                     conditionalPanel(condition = "input.time2 =='day_of_the_week_visit'",
                                                      checkboxGroupInput("day2", "Select weekday", choices = c('Monday',
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
                                                      h5(strong("timespan selection :")),
                                                      numericInput("hours12","between ",value=0, min =0, max =23, step = 1),
                                                      numericInput("hours22"," and ",value=23, min =0, max =23, step = 1),
                                                      h5(strong("hours"))
                                     ),

                                     actionButton("refresh2",label = "Refresh ")
                                    ),
                 mainPanel(
                 plotOutput("plot_time2",width = "180%",height = "600px"),
                 textOutput("number_visitors_period2")

               )
              ),
      tabPanel("Map",
               sidebarPanel(
                 h5(strong("user can zoom in and find the location of all the restaurants. For each restaurant, we can find its id
                    and its genre (type of restaurant) as well as its location of course"))
                            ),
               mainPanel(
                 leafletOutput("map")

                        )


              ),
      tabPanel("Analyse by Genre/region",
               sidebarPanel(strong("Output selection "),radioButtons("time4", "Select a timespan :", choices=c("by day in the year"="visit_date",
                                                                                             "by month"="visit_month")
                                                ),
                                    conditionalPanel(condition = "input.time4 =='visit_date'",
                                             dateRangeInput("daterange4", "Date range:",
                                                            start = as.Date('2016-01-01'),
                                                            end = as.Date('2017-05-31'),
                                                            min=as.Date('2016-01-01'),
                                                            max=as.Date('2017-05-31')
                                                    )
                                                ),
                                    conditionalPanel(condition = "input.time4 =='visit_month'",
                                                 checkboxGroupInput("month4",
                                                                    "Select months",
                                                                    choices = unique(df$visit_month),
                                                                    selected=unique(df$visit_month)
                                                                    )
                                                 ),
                                    checkboxGroupInput("Area4","Select area you're interested in:",choices=unique(df$area_name)),
                                    checkboxGroupInput("Genre4","select genre you're interested in:",choices=unique(df$genre_name)),
                                    actionButton("refresh4",label = "Refresh")

                          ),
               mainPanel(
                         plotOutput("plot_time4",width = "180%",height = "600px"),
                         textOutput("number_visitors_period4")
                        )

              ),
      tabPanel("Prediction",
               sidebarPanel(strong("Output selection"),
                            selectizeInput("id5",label = "Choose IDs for prediction ",choices = unique(prediction$id), options = list(maxItems = 25)),
                            dateInput("date5","Choose you date:",value = as.Date('2017-04-23'),min=as.Date('2017-04-23'),max=as.Date('2017-05-31')),
                            actionButton("refresh5",label = "Refresh")
                            ),
               mainPanel(
                          DT::DTOutput("Table5")
                        )

              ),
      tabPanel("Error Evaluation",
               sidebarPanel(strong("Output selection"),
                            numericInput("n6","choose number of predictions ",value=50, min =1, max =200, step = 1),
                            actionButton("refresh6",label = "Refresh")

                            ),
               mainPanel(
                        plotOutput("graph6",width = "180%",height = "600px")
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "none",plot.title = element_text(hjust = 0.5))+
      ggtitle("Number of visitors versus time span under the output selection constraints") +
      xlab("Time span") +
      ylab("Number of Visitors")
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

  graph_dif <- function(test,n){
    test <- test[round(50000/n)*c(1:n), ]
    test <- test %>% mutate(y = y_true - xgbpred)
    mean <- mean(test$y)
    ggplot(data = test, aes(id, y)) +
      geom_point(aes(id,y)) +
      theme_minimal() +
      ggtitle(strong("Difference between real and predicted number of visitors")) +
      geom_hline(yintercept=mean,linetype = "dashed",color="red")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5),legend.position = "none",plot.title = element_text(hjust = 0.5))
  }



   pp1 <- eventReactive(input$refresh,{general_overview(df,input$time,input$daterange)})
   pp2 <- eventReactive(input$refresh,{count(df,input$time,input$daterange)})
   pp12 <- eventReactive(input$refresh2,{personal_overview(df,input$time2,input$daterange2)})
   pp22 <- eventReactive(input$refresh2,{personal_count(df,input$time2,input$daterange2)})
   pp14 <- eventReactive(input$refresh4,{area_genre_overview(df,input$time4,input$daterange4)})
   pp24 <- eventReactive(input$refresh4,{area_genre_count(df,input$time4,input$daterange4)})
   pp15 <- eventReactive(input$refresh5,{predictive_table(prediction,input$id5,input$date5)})
   pp16 <- eventReactive(input$refresh6,{graph_dif(test.prediction,input$n6)})

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
   output$graph6 <- renderPlot({
     pp16()
   })
}

# Run the application
shinyApp(ui = ui, server = server)

