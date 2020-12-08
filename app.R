library(tidyverse)     # for data cleaning and plotting
library(googlesheets4) # for reading googlesheet data
library(lubridate)     # for date manipulation
library(openintro)     # for the abbr2state() function
library(palmerpenguins)# for Palmer penguin data
library(maps)          # for map data
library(ggmap)         # for mapping points on maps
library(gplots)        # for col2hex() function
library(RColorBrewer)  # for color palettes
library(sf)            # for working with spatial data
library(leaflet)       # for highly customizable mapping
library(ggthemes)      # for more themes (including theme_map())
library(plotly)        # for the ggplotly() - basic interactivity
library(gganimate)     # for adding animation layers to ggplots
library(gifski)        # for creating the gif (don't need to load this library every time,but need it installed)
library(transformr)    # for "tweening" (gganimate)
library(shiny)         # for creating interactive apps
library(patchwork)     # for nicely combining ggplot2 graphs  
library(gt)            # for creating nice tables
library(rvest)         # for scraping data
library(robotstxt)     # for checking if you can scrape data
library(paletteer)
gs4_deauth()           # To not have to authorize each time you knit.
theme_set(theme_minimal())

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

states_name<-covid19%>%
  distinct(state)%>%
  arrange(state)%>%
  pull()
ui <- fluidPage(
  selectInput("state", "State",
              choices = states_name), 
  multiple = TRUE,
  submitButton(text = "Finish!"),
  plotOutput(outputId = "timeplot"))

server <- function(input, output) {
  output$timeplot<-renderPlot({covid19%>%
      filter(state%in%input$state)%>%
      filter(cases>=20)%>%
      mutate(days_since_20 = date-min(date))%>%
      ggplot()+
      labs(x="days after first 20 cases",
           y="cases")+
      geom_line(aes(x = days_since_20,
                    y = cases, col = state))+
      scale_y_log10()+
      facet_wrap(vars(state))+
      theme_minimal()})}
shinyApp(ui = ui, server = server)
