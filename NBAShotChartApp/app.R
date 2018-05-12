#Load libraries
library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(readr)
library(jpeg)
library(tidyr)
library(png)

#Load data
player_stats <- read.csv('player_shots.csv', stringsAsFactors = FALSE)
filenames <- read.csv('jpegFilenames.csv', stringsAsFactors = FALSE)

player_stats <- player_stats %>%
  mutate(shot_made_flag = as.factor(shot_made_flag))

# Use fluid page layout (rows and columns)
ui<-fluidPage(
  includeCSS("style.css"),
  
  #Title
  h1("Shot Charts for NBA Players", style = "color:white"),
  p("We were interested in seeing how well the top 25 NBA players perform. We look at different areas of the court and can see where they made the shot from and in different quarters or for an entire season. We downloaded and cropped our court images from ", a("Sports Illustrated", href="https://www.si.com/nba/photo/2016/09/16/top-25-nba-players-2017"), ". We downloaded our player data from ", a("NBA Savant", href="http://nbasavant.com/shot_search.php"), " and downloaded the NBA schedule from ", a("Kaggle", href="https://www.kaggle.com/pablote/nba-enhanced-stats"), ". Finally, we scraped the NBA abbreviations from ", a("Wikipedia", href="https://en.wikipedia.org/wiki/Wikipedia:WikiProject_National_Basketball_Association/National_Basketball_Association_team_abbreviations"), " which helped us match a lot of our data.", style = "color:white"),
  
  
  
  #Generate row with sidebar
  sidebarLayout(
    
    #Define sidebar
    sidebarPanel(
      id = "controls",
      helpText("NBA Player Data"),
      
      selectInput(inputId = "player_selection",
                  label = "Select Player",
                  choices = unique(player_stats$name),
                  selected = "LeBron James"), 

      selectInput(inputId = "scope_selection",
                  label = "Game or Season",
                  choices = c("Individual Game(s)", "Season"),
                  selected = "Season"),
      
      conditionalPanel("input.scope_selection == 'Individual Game(s)'",
                       selectizeInput(inputId = "game_selection",
                                   label = "Game(s)",
                                   choices = "",
                                   selected = "",
                                   multiple = TRUE)),

      checkboxInput("advancedOptions", "Advanced Options"),
      
      conditionalPanel("input.advancedOptions == true",
                       checkboxGroupInput(inputId = "shot_type",
                                   label = "Breakdown by Shot Value",
                                   choices = unique(player_stats$shot_type),
                                   inline = TRUE,
                                   selected = unique(player_stats$shot_type)),
                       
                       sliderInput(inputId = "quarter",
                                   label = "Breakdown by Quarter",
                                   min = 1,
                                   max = 4,
                                   value = c(1,4))),
      tags$head(tags$style("#scatterplot{height:70vh !important;}"))
    ), # end side bar panel
    
    #Put output graphic in main panel
    mainPanel(
      style="background-color : 	#808080;   opacity: 0.95; color: #FFFFFF",
      plotOutput(outputId = "scatterplot"),
      textOutput(outputId = "text")

    )
  )  
)

##### Server Side #####
server<-function(input, output, session){

  outVar = reactive({
      unique(filter(player_stats, name == input$player_selection)$game_date_detailed)
  })
  
  observe({
    updateSelectizeInput(session, "game_selection",
                      choices = outVar(),
                      selected = outVar()[1])
    })
  
  #Create reactive dataset
  selectedData <- reactive({
    filter_stats <- filter(player_stats, name == input$player_selection)
    
    # If the user chooses to filter by game:
    if(input$scope_selection == "Individual Game(s)"){
      filter_stats <- filter(filter_stats, game_date_detailed %in% input$game_selection)
    }
    
    # If the user chooses advanced options
    if(input$advancedOptions == TRUE){
      filter_stats <- filter_stats %>%
      filter(shot_type %in% input$shot_type,
             period >= input$quarter[1],
             period <= input$quarter[2])
    }
    # pass on filter_stats to selectedData
    filter_stats
  })
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
      
      # filter for the selected player
      filter_stats <- filter(player_stats, name == input$player_selection)
      
      # filter for selected games
      if(input$scope_selection == "Individual Game(s)"){
        filter_stats <- filter(filter_stats, game_date_detailed %in% input$game_selection)
      }
      
      # get the home court if the season long stats is selected
      if(input$scope_selection == "Season"){
       court_name <- filter_stats[1,25]
      }
      else{
        if(length(input$game_selection) != 0){
          court_name <- filter_stats[1,28]
        } else{
          filter_stats <- filter(player_stats, name == input$player_selection)
          court_name <- filter_stats[1,25]
        }
      }
      
      # If multiple games are selected, default to home court
      if(length(input$game_selection) > 1){
        court_name <- filter_stats[1,25]
      }    
      
      court_filename <- filter(filenames, Abbr == court_name)[1,2]
      court <- readJPEG(court_filename[[1]])
      
      ggplot(data = selectedData(), aes(x = x, y = y, fill = shot_made_flag)) + 
        coord_fixed() + 
        annotation_raster(court, ymin = -4, ymax= 90,xmin = -25,xmax = 25) +
        geom_point(aes(color = shot_made_flag, shape = shot_made_flag), alpha = .6, size = 5) +
        guides(fill=FALSE) +
        scale_shape_manual("Shots Missed/Made:", labels = c("Missed", "Made"), values = c(4,16)) +
        scale_color_manual("Shots Missed/Made:", labels = c("Missed", "Made"), values= c("#CC0000","#00CC33")) +
        xlim( -25, 25) +
        ylim(0, 90) + 
        theme_void() +
        theme(
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank()
      )
  
  }, bg = "transparent")
  
  #Create text output 
  output$text <- renderText({
    #Wrangle 
    shots_attempted <- sum(as.numeric(selectedData()$shot_made_flag))
    if(input$scope_selection == 'Season'){
      unit = " season"
    } else{
      unit = " games"
    }
    #Text
    paste(input$player_selection, " attempted ", shots_attempted, 
           " shots in ", length(input$game_selection), unit)
           
    
  })
  
}

shinyApp(ui=ui, server=server)   