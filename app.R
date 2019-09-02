library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rvest)
library(wordcloud)
library(tm)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(NLP)
library(dplyr)
library(RColorBrewer)
library(scales)
library(mosaic)
library(ggplot2)
library(dplyr)
library(ggmap)
library(leaflet) 
library(tigris)
library(tidyverse)
library(plotly)
library(plyr)
library(knitr)
library(ggthemes)
library(stringr)
library(grid)
library(gridExtra)
source("helpers.R")
load("nba_shots.RData")
gg_court = make_court()
players = nba_shots %>% distinct(player_name) %>% pull()
made = nba_shots %>% distinct(shot_made_flag ) %>% pull()
ui <- dashboardPagePlus(  
  skin = "blue", # change the apperance of app, it makes the top bar gray and other parts black 
  header = dashboardHeaderPlus(
    title = "NBA Champion Study",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "info"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Description", 
        tabName = "page1", 
        icon = icon("text-size", lib="glyphicon")
      ),
      menuItem(
        text = "Information", 
        tabName = "page2", 
        icon = icon("home", lib="glyphicon")
      ),
      menuItem(
        text = "Shot Attempts", 
        tabName = "page5",
        icon=icon("cloud")
      ),
      menuItem(
        text = "Salary Analysis", 
        tabName = "page7",
        icon=icon("money")
      ),
      menuItem(
        text = "Map Visualization", 
        tabName = "page3",
        icon=icon("table")
      ),
      menuItem(
        text = "Interactive Map", 
        tabName = "page4",
        icon=icon("gears")
      ),
      menuItem(
        text = "Conclusion", 
        tabName = "page6",
        icon=icon("school")
      ),
      hr() # adds an horizental line like a divider

    
    )
  ),
  body = dashboardBody(
    setBackgroundImage(src = "nba-logo"),
    tabItems(
      tabItem(
        tabName="page4",
        strong("Interactive map created based on Leaflet"),
        fluidRow(
          column(width=12,leafletOutput("plot8", height=500),
                 fluidRow(verbatimTextOutput("map_marker_click")))
        )
      ),
      tabItem(
        tabName="page3",
        column(12,
          fluidRow(
            column(width=6,plotOutput("plot4", height=400)),
            column(width=6,plotOutput("plot5", height=400))
          ),
          fluidRow(
            column(width=6,plotOutput("plot6", height=400)),
            column(width=6,plotOutput("plot7", height=400))
          )
        )
      ),
      tabItem(
        tabName="page6",
        column(12,
        img(src="100.png",width="100%"),
        img(src="101.png",width="100%"),
        img(src="102.png",width="100%"),
        img(src="103.png",width="100%"),
        img(src="104.png",width="100%"),
        img(src="105.png",width="100%"),
        img(src="106.png",width="100%"),
        img(src="107.png",width="100%"),
        img(src="108.png",width="100%"),
        img(src="109.png",width="100%"),
        img(src="110.png",width="100%"),
        img(src="111.png",width="100%"),
        img(src="112.png",width="100%")
        )
      ),
      tabItem(
        tabName="page7",
        fluidRow(
          column(width=12,plotOutput("plot10", height=900))
        ),
        br(),
        br(),
        br(),
        fluidRow(
          column(9,plotlyOutput("plot11", height=400)),
          column(3, 
                 sliderInput("yearSlider", "Year:", 
                             min = 1990, max = 2017, value = 1990, step = 1,
                             animate = animationOptions(interval = 1000, loop = TRUE))
                 )
        )
      ),
      tabItem(
        tabName="page5",
        # Application title
        fluidPage(
        titlePanel("NBA Shot Attempts"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
          sidebarPanel(
            
            # # drop down menu for player
            selectInput("player_choice", label = h2("Select player"),
                        choices = players, selected = "LeBron James") , 
            # # drop down menu for season based on a certain player
            uiOutput("season_choice") ,
            
            radioButtons("shots_made", label = h3("Shot status"), choices = list("all", "made", "missed"))
            
          ),
          
          # Show output based on user selections
          mainPanel(
            
            # # spatial plot of shots made
            plotOutput("court_shots") ,
            # 
            # # box plot of shot distances
            plotlyOutput("shot_distances")
            
          )
        )
      )
      ),
      
      tabItem(
        tabName = "page1",
        fluidRow(
          column(1),
          column(10,        
                 img(src="las-vegas-summer-league-2018.jpg",width="100%"),
        h2("Description"),
        p("Our team want to use knowledge on data visualization to analyze factors associated with NBA champions."),
        br(),
        fluidRow(
        column(6,
                 h3("Data source"),
                 p("The data of NBA we use are found on some NBA website, such as official NBA website, Basketball Reference 
                   and Kaggle. The map data we use were from the dataset for Week 6 and internal dataset of US map"
                 ),
                 br(), # adds an empty line
                 h3("Methods"),
                 p("The main methods in this small project are Mapping Visualization, leaflet interactive map, Chart Comprision, Web Scrapping, Text Mining, and Wordcloud Visualization."),
                 br(),
                 
                 h3("Users"),
                 p("Users are able to compare the champions in different years with the associated factors. There is also a wordcloud designed in this page that helps users learn more abourt the teams on their own interest."),
                 # we create an unorder list using ul and li.
                 tags$ul(
                   tags$li("Shot attempts"),
                   tags$li("Different factors associated with champion rate"),
                   tags$li("Interactive map related with player numbers")
               )
        ),
        column(6,
        box(width = 12,
                 br(),
                 h3("Map Visualization"),
                 p("We use webscrapping methods to collect all the words from the wikipedia websites."),
                 br(),
                 h3("Interactive Map"),
                 p("Interactive map can help us know the player number of each state."),
                 br(),
                 h3("Shot Attempts"),
                 p("In this page, we will show the NBA shot attempts in a graphic."),
                 br(),
                 h3("Salary Analysis"),
                 p("We analyze the correlationship between players' salaries and their performance on
                   the court. Then, we select 10 top-salary players, and make a graph to compare their salaries."))
        )))
      )),
      tabItem(
        tabName = "page2",
        fluidRow(
          column(1),
          column(10,
        fluidRow(
          column(3,
                 img(src='jhu.jpg', align = "left", width= "100%")
                 ),
         column(5,
                h4("Data Visualization Team"),
                h5("Hanchi Gu" ),
                h5("Limou Xie" ),
                h5("Junyi Xu" ),
                h5("Yongda Xu" ),
                p("Four teammates made this website to study various factors related to NBA champions")
                ),
         column(4,img(src='group.jpg', align = "left", width= "100%"))
         
         ),
        fluidRow(
            column(6,
                   h3("Champions for every year"),
                                   sliderInput("yearSlider1","Year",
                                               min=1947,max=2019,value=2008,step=1,
                                               animate=animationOptions(interval=600,loop=TRUE)
                                               
                                   )
                   ),
            column(3),
            column(3,uiOutput("text2"))

              
            ),
          fluidRow(
            h3("Know more about NBA"),
            
            
            column(
              width = 4,
              tabBox(
                width = 12,
                title = "Toolbox",
                id = "tabset1", height = "250px",
                tabPanel(
                  title = "Team",
                  textInput(
                    inputId = "input1",
                    label = "Team name", 
                    value = "lakers"),
                  actionButton('Botton1','show')
                ),
                tabPanel(
                  title = "Details",
                  sliderInput("num_of_words", "Number of words",
                              min = 0, max = 50,
                              value = 25),
                  radioButtons("method", "Scale method",
                               c("sqrt" = "sqrt",
                                 "log2" = "log2",
                                 "log" = "log",
                                 "none" = "none")),
                  selectInput("pallet", "Color pallet",
                              c("Set1" = "Set1",
                                "Set2" = "Set2",
                                "Set3" = "Set3",
                                "Dark2" = "Dark2",
                                "Accent" = "Accent"))
                )
              )
            ),
            column(
              width = 8,
              box(width = 12,
                  plotOutput(
                    outputId = "plot1", 
                    height=450
                  )
              )
            )
            
            
          ), 
          
        fluidRow(
        
        hr(),
        h3("Websites"),  
        p("You can use the following websites for reference"),
        p(span("NBA official website"),
          a(href="https://www.nba.com/#/", target="_blank", "NBA official website")),
        p(span("NBA twitter"),
          a(href="https://twitter.com/search?q=NBA&ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Esearch", target="_blank", "NBA twitter")),
        p(span("NBA news"),
          a(href="https://www.nba.com/news", target="_blank", "NBA news"))
        ),
        
        fluidRow(
          
          # Copy the chunk below to make a group of checkboxes
          checkboxGroupInput("checkGroup", label = h3("FAQs"), 
                             choices = list("When is the data collected?" = "The dataset include data before 2019", "What is the aim of this page?" = "Help you understand the factors that make a team champion", "How to contact with us?" = "jxu90@jhu.edu"),
                             selected = 1),
          
          
          hr(),
          fluidRow(column(12, verbatimTextOutput("value"),height= 120))
          
        )
        
          )
        )
        
      )
    )
  ),
  rightsidebar = rightSidebar(
p("NBA teams"),
 img(src="1.jpg", width="100%"), 
img(src="2.jpg", width="100%"), 
img(src="3.jpg", width="100%"), 
img(src="4.jpg", width="100%"), 
img(src="5.jpg", width="100%"), 
img(src="6.jpg", width="100%"), 
img(src="7.jpg", width="100%"), 
img(src="8.jpg", width="100%"), 
img(src="9.jpg", width="100%") 

  ),# for this project we did not put anything inside the right side bar
                footer = dashboardFooter(
                  left_text = p(
                    icon("user", lib="glyphicon"), #icon acts like a text 
                    "User"
                  ),
                  right_text = p(
                    icon("envelope"), #the default lib for icons is font-awesome
                    "Data Visualization"
                  )
                )
              )
              
              
server <- function(input, output,session) {
                updateTextInput(session, "input1", 
                value = "lakers")
  
                wikiWebScraper <- function(key) {
                  myStopWords <- c("I","may", "now", "also", "many", "use", "used", "typically","given",
                                   "like", "will", "can", "often", "see", "one", "pdf", "issn", "journal",
                                   tolower(month.name))
                  
                  v <-  paste0("https://en.wikipedia.org/wiki/",key) %>% 
                    read_html %>% html_nodes("#bodyContent") %>% html_text %>% 
                    VectorSource %>% Corpus %>% tm_map(content_transformer(tolower)) %>% 
                    tm_map(removeNumbers) %>% tm_map(removeWords, stopwords("english")) %>% 
                    tm_map(removeWords, myStopWords) %>% tm_map(removePunctuation) %>% 
                    tm_map(stripWhitespace) %>% TermDocumentMatrix %>% as.matrix %>% 
                    rowSums %>% sort(decreasing=TRUE)
                  
                  if(input$method=="none") {d <- data.frame(word = names(v),freq=v)}
                  else if(input$method=="sqrt") {d <- data.frame(word = names(v), freq=round(sqrt(v)))}
                  else if(input$method=="log2") {d <- data.frame(word = names(v), freq=round(log2(v)))}
                  else if(input$method=="log") {d <- data.frame(word = names(v), freq=round(log(v)))}
                  
                  
                  set.seed(5)
                  wordcloud(words = d$word, freq = d$freq, max.words=input$num_of_words, 
                            random.order=FALSE, rot.per=0.35, min.freq = 1,
                            colors=brewer.pal(8,input$pallet))
                }
                
                output$plot1 <- renderPlot({
                  input$Button1
                  wikiWebScraper(isolate(input$input1))
                })
                output$value <- renderPrint({ input$checkGroup })
                #####
                nba_data<-read.csv("nba.csv")
                us_states<-read.csv("us_states.csv")
                nba_data$region <- tolower(nba_data$state_name)
                nba_map<- left_join(us_states, nba_data)
                output$plot4 <- renderPlot({
                p <- ggplot(data = nba_map,
                            mapping = aes(x = long, y = lat, fill = level, 
                                          group = group)) + 
                     geom_polygon(color = "gray90", size = 0.05) + coord_equal() +
                     scale_fill_manual(values=c("#FFFF00", "#FFFF66", "#FFFF99","#FFFFCC","#FFFFFF"))+
                    guides(fill = guide_legend(nrow = 1)) + 
                    theme_map()+
                    theme(legend.position = "bottom")+
                    labs(title="Championships")
                p
                })
                map_data <- read.csv("county_map.csv")
                indicators_data <- read.csv("county_data.csv")
                full_data <- left_join(map_data, indicators_data, by = "id")
                output$plot5<-renderPlot({
                p <- ggplot(data = full_data,
                              mapping = aes(x = long, y = lat, fill = pct_black, 
                                            group = group)) + 
                    geom_polygon(color = "white", size = 0.05) + coord_equal() + scale_fill_brewer(palette="Greys",
                                                                                                   labels = c("0.0-2.0", "2.0-5.0", "5.0-10.0", "10.0-15.0",
                                                                                                              "15.0-25.0", "25.0-50.0", "50.0-85.3"))+
                    theme_map() +
                    theme(legend.position = "bottom")+
                    labs(title="Percentage of African American")
                p
                })
                map_data <- read.csv("county_map.csv")
                indicators_data <- read.csv("county_data.csv")
                full_data <- left_join(map_data, indicators_data, by = "id")
                output$plot6<-renderPlot({
                p <- ggplot(data = full_data,
                              mapping = aes(x = long, y = lat, fill = pct_black, 
                                            group = group)) + 
                    geom_polygon(color = "white", size = 0.05) + coord_equal() + scale_fill_brewer(palette="Greys",
                                                                                                   labels = c("0.0-2.0", "2.0-5.0", "5.0-10.0", "10.0-15.0",
                                                                                                              "15.0-25.0", "25.0-50.0", "50.0-85.3"))+
                    theme_map() +
                    theme(legend.position = "bottom")+
                    labs(title="Percentage of African American")
                p + aes( fill = hh_income) + 
                  theme_map() +
                  scale_fill_gradient(low = "white", high = "dark green") + labs(fill = "Percent", title = "House Hold Income")
                })
                map_data <- read.csv("county_map.csv")
                indicators_data <- read.csv("county_data.csv")
                full_data <- left_join(map_data, indicators_data, by = "id")
                full_data$gdp<-(full_data$pop)*full_data$hh_income/100
                output$plot7<-renderPlot({
                p <- ggplot(data = full_data,
                              mapping = aes(x = long, y = lat, fill = pct_black, 
                                            group = group)) + 
                    geom_polygon(color = "white", size = 0.05) + coord_equal() + scale_fill_brewer(palette="Greys",
                                                                                                   labels = c("0.0-2.0", "2.0-5.0", "5.0-10.0", "10.0-15.0",
                                                                                                              "15.0-25.0", "25.0-50.0", "50.0-85.3"))+
                    theme_map() +
                    theme(legend.position = "bottom")
                p + aes(fill = full_data$gdp) + 
                    scale_fill_gradient(low = "white", high = "dark red") + labs(fill = "Percent", title = "")+
                    labs(title = "GDP", fill = "percent")
                })
                statefreq=read.csv("statefreq.csv")
                states <- states(cb=T)
                states_merged_sb <- geo_join(states, statefreq, "STUSPS", "state")
                pal <- colorNumeric("Greens", domain=states_merged_sb$number)
                states_merged_sb <-subset(states_merged_sb,!is.na(number))
                popup_sb <- paste0("Total: ", as.character(states_merged_sb$number))
                output$plot8<-renderLeaflet({
                p<-leaflet() %>%
                    addTiles()%>%
                    ##addProviderTiles("CartoDB.Positron") %>%
                    setView(-98.483330, 38.712046, zoom = 4) %>% 
                    addPolygons(data = states_merged_sb , 
                                fillColor = ~pal(states_merged_sb$number), 
                                fillOpacity = 0.7, 
                                weight = 0.2, 
                                smoothFactor = 0.2, 
                                popup = ~popup_sb) %>%
                    addLegend(pal = pal, 
                              values = states_merged_sb$number, 
                              position = "bottomright", 
                              title = "Birthplace")
                p
                })
                data <- read.csv("Champ.csv")
                subData<-reactive({data[data$Year==input$yearSlider1,]})
                output$text2 <- renderUI({
                  wellPanel(
                    p(strong("Champion")),
                    img(src=paste0(subData()$Team[1], ".png"), width="100%"),
                    h4(subData()$Team[1]),
                    br()
                  )
                  
                })
                output$season_choice <- renderUI({
                  seasons = nba_shots %>% filter(player_name == input$player_choice) %>% 
                    distinct(season) %>% pull()
                  
                  selectizeInput("season_choice", label = h3("Select season"), choices = seasons,
                                 selected = seasons[1], multiple = TRUE)
                })
                # 
                output$court_shots <- renderPlot({
                  #   # subset data by selected player and season(s)
                  player_data = filter(nba_shots, player_name == input$player_choice,
                                       season %in% input$season_choice)
                  # 
                  #   # create plot
                  gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                                        aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
                    scale_color_manual("", values = c(made = "blue", missed = "orange"))
                })
                # 
                # 
                output$shot_distances <- renderPlotly({
                  nba_shots %>%
                    filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
                    plot_ly(y = ~shot_distance, color = ~player_name, type = "box") %>%
                    layout(showlegend = FALSE)
              
})
                salaries<-read.csv('player_salary.csv')
                season_stats <-read.csv("Seasons_Stats.csv") 
                season_stats <- season_stats %>% select(-c(blanl, blank2))
                salaries_2017 <- salaries %>% filter(Season.End==2017)
                season_stats_2017 <- season_stats %>% filter(Year==2017)
                season_stats_2017$Pos <- factor(season_stats_2017$Pos)
                season_stats_2017$Player <- factor(season_stats_2017$Player)
                season_stats_2017$Tm <- factor(season_stats_2017$Tm)
                salaries_2017$Player.Name <- factor(salaries_2017$Player.Name)
                salaries_2017$Team <- factor(salaries_2017$Team)
                salaries_2017$Team <- revalue(salaries_2017$Team, c("CHA"="CHO", "NJN"="BRK", "NOH" = "NOP"))
                season_stats_2017 <- season_stats_2017 %>%
                  mutate(PPG = PTS/G, APG = AST/G, RPG = TRB/G, 
                         BPG = BLK/G, SPG = STL/G, MPG = MP/G, 
                         TOPG = TOV/G, PFPG = PF/G )
                
                salaries_2017_join <- salaries_2017 %>% 
                  select(c(Player.Name, Team, Salary))
                stats_with_salaries <- inner_join(season_stats_2017, salaries_2017_join,
                                                  by = c('Player' = 'Player.Name', "Tm"="Team"))
                subset <- stats_with_salaries %>%
                  select(c(Player,Tm, Pos, Age,  G, GS, MPG, PPG, APG, RPG, BPG,
                           SPG, TOPG, PFPG, WS, PER, VORP, X2P., X3P., FG., TS., 
                           USG., Salary ) )
                output$plot10 <- renderPlot({       
                  p1 <- ggplot(subset, aes(x=Salary, y=PPG)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y="Points PG") +
                    theme(legend.position="none") +
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  p2 <- ggplot(subset, aes(x=Salary, y=MPG)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y="Minutes PG") +
                    theme(legend.position="none")+
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  p3 <- ggplot(subset, aes(x=Salary, y=GS)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y="Games Started") +
                    theme(legend.position="none")+
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  p4 <- ggplot(subset, aes(x=Salary, y=PFPG)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y="Personal Fouls PG") +
                    theme(legend.position="none")+
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  p5 <- ggplot(subset, aes(x=Salary, y=RPG)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y= "Rebounds PG") +
                    theme(legend.position="none")+
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  p6 <- ggplot(subset, aes(x=Salary, y=Age)) +
                    geom_point(alpha=0.5) + geom_smooth(method=lm,fullrange=TRUE) +
                    labs(x="Salary", y="Age") +
                    scale_x_continuous(breaks = c(100000, 10000000,
                                                  20000000, 30000000),
                                       labels = c("$1M", "$10M",
                                                  "$20M", "$30M"))+
                    theme_classic() +
                    theme(
                      plot.margin = margin(3, 7, 3, 1.5)
                    )
                  
                  grid.arrange(p1, p2, p3, p4, p5, p6,
                               layout_matrix=cbind(c(1,4), c(2,5), c(3,6)), 
                               top = textGrob("Predictors related to the Salary",
                                              gp=gpar(fontsize=20,font=3)))
                  
  })    
                salaries<-read.csv('player_salary.csv')
                season_stats <-read.csv("Seasons_Stats.csv")
                df<-reactive({
                  subsalaries<-salaries[salaries$Season.Start==input$yearSlider,]
                  sort_by_salary<-subsalaries[order(-subsalaries$Salary),]
                  highest_salary<-sort_by_salary[1:10,]
                  highest_salary$Player.Name <- factor(highest_salary$Player.Name, levels = highest_salary$Player.Name[order(-highest_salary$Salary)])
                  highest_salary
                  })
                
                output$plot11 <- renderPlotly({
                  p<- ggplot(df(), aes(x=Player.Name,y=Salary,fill=-Salary))+
                    geom_bar(stat="identity")
                  p
                })
   }
                
shinyApp(ui = ui, server = server)

