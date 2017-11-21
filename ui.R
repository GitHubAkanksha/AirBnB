
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggmap)
library(ggplot2)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  headerPanel("Boston Air BnB Listing Visualizations"),
  
  sidebarPanel
  (
    selectInput("Attributes", "Select Attribute to Visualize", 
                choices = c("Property Type","Location", "Room Type","Rate")),
    uiOutput("selectionOnFly"),
    conditionalPanel(condition = "input.Attributes == 'Rate'",
                     sliderInput("InputRate", "Select range for Rates (USD)", 
                                 min = 0, max = 500, value = 10, step = 1))
    
  ),
  mainPanel(
    
    tabsetPanel(type = "tabs", id="tabSetPanel", 
                tabPanel("Leaflet", leafletOutput("leafletMap", height = 700)),
                tabPanel("Plot", plotOutput("listingplot")),
                tabPanel("Summary",verbatimTextOutput("summarytable")),
                tabPanel("Review",plotOutput("ReviewOut")),
                tabPanel("Sentiments",plotOutput("sentimentPlot"))
                
    )
  )
))
