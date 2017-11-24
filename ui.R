
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  # Application title
  navbarPage("Boston Air BnB Listing Visualizations",
             
    tabPanel("Visualization",
      sidebarLayout(
        sidebarPanel
        (
          selectInput("Attributes", "Select Attribute to Visualize", 
                      choices = c("Property Type","Location","Rate")),
          uiOutput("selectionOnFly"),
          conditionalPanel(condition = "input.Attributes == 'Rate'",
                           sliderInput("InputRate", "Select range for Rates (USD)", 
                                       min = 11, max = 1300, value = 20, step = 1))
          
        ),
        mainPanel(
          
          tabsetPanel(type = "tabs", id="tabSetPanel", 
                      
                      tabPanel(id="map", "Map", leafletOutput("leafletMap", height = 700)),
                      
                      tabPanel(id = "sumamry_statistics", "Summary Statistics",
                               fluidRow(column(6, plotOutput("plot1",hover = 'Price')),
                                        column(6, plotOutput("plot2",hover = 'Number of Bedrooms')),
                                        column(6, plotOutput("plot3",hover = 'Number of Bathrooms')),
                                        column(6, plotOutput("plot4",hover = 'Number of Reviews')))
                      ),
                      
                      tabPanel(id = "description", "Description",plotOutput("ReviewOut", height = 700)),
                      tabPanel(id = "sentiments", "Sentiments",
                               plotOutput("dynamicSentimentPlot", width = "100%", height = 700)
                      )
                      
          )
        )
      )
    ),
    
    tabPanel("Top Neighbourhoods",
             plotOutput("sentimentPlot", width = "70%", height = 800)
    ),
    
    tabPanel("Influential Predictors",
             verbatimTextOutput("Regression")
        )
    )
  )
)
