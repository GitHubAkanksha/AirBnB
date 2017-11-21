
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggmap)
library(ggplot2)
library(rlist)
library(leaflet)
library(RColorBrewer)
library(readr)
library(SnowballC)
library(wordcloud)
library(tm)
library(dplyr)
library(data.table)

# Get current working directory
workingDir <- paste(getwd(),"Data", sep="/")

# Names of datasets
listingsDSName = "listings"
calendarDSName = "calendar"
reviewsDSName = "reviews"

colNamesListings = c("id", "name", "description", "summary", "neighbourhood_cleansed", 
                     "latitude", "longitude", "property_type", "room_type", "accommodates", 
                     "bathrooms", "bedrooms", "beds", "amenities", "price", 
                     "number_of_reviews", "review_scores_rating", 
                     "review_scores_location", "instant_bookable")

# Read datasets in R environment - Region Start

#listings <- read.csv(file=paste(paste(workingDir, listingsDSName, sep = "/"),'.csv',sep=''), header=TRUE, sep = ",", stringsAsFactors = FALSE)
#reviews <- read.csv(file=paste(paste(workingDir, reviewsDSName, sep = "/"),'.csv',sep=''), header = TRUE, sep = ",")

listings <- fread(input=paste(paste(workingDir, listingsDSName, sep = "/"),'.csv',sep=''), sep = ",", 
                  header = "auto", select = colNamesListings)

# Read datasets in R environment - Region End

shinyServer(function(input, output, session) {
  
  # Declaring & Initializing global variables - Region Start
  
  outputType <- list(o1 = list(type='Plot',variablesOfInterest=c("latitude","longitude","property_type","neighbourhood_cleansed","price")),
                     o2 = list(type='Summary',variablesOfInterest=c("bedrooms","bathrooms","number_of_reviews","price")),
                     o3 = list(type='Leaflet',variablesOfInterest=c("latitude","longitude","property_type","neighbourhood_cleansed","price")),
                     o4 = list(type='Review',variablesOfInterest=c("summary"))
                     )
  
  inputAttr <- list(a1 = list(type='Property Type',varName="property_type", inputControlName="InputPropertyType", plotColor="red"),
                    a2 = list(type='Location',varName="neighbourhood_cleansed", inputControlName="InputLocation", plotColor="blue"),
                    a3 = list(type='Room Type',varName="room_type", inputControlName="InputRoomType", plotColor="cyan"),
                    a4 = list(type='Rate',varName="transformed_price", inputControlName="InputRate", plotColor="green")
                    )
  
  varsOfInterest=c("")
  varNameToFilter = c("")
  ipControlName = c("")
  varColor = c("")
  
  # Declaring & Initializing global variables - Region End
  
  
  
  # Read datasets in R environment - Region Start
  
  listingLocations <- sort(as.vector(unique(listings$neighbourhood_cleansed)), decreasing = FALSE)
  listingPropertyTypes <- sort(as.vector(unique(na.omit(listings$property_type[listings$property_type != ""]))), decreasing = FALSE)
  listingRoomTypes <- sort(as.vector(unique(na.omit(listings$room_type[listings$room_type != ""]))), decreasing = FALSE)
  
  # Read datasets in R environment - Region End
  
  
  # Generating HTML controls on fly in ui.R - Region Start
  
  output$selectionOnFly <- renderUI({
    if(!is.null(input$Attributes)) {
      
      if(input$Attributes == "Property Type")
      {
        selectInput("InputPropertyType", "Select Property Type", choices = listingPropertyTypes)
      }
      else if(input$Attributes == "Location")
      {
        selectInput("InputLocation", "Select Location", choices = listingLocations)
      }
      else if(input$Attributes == "Room Type")
      {
        selectInput("InputRoomType", "Select Room Type", choices = listingRoomTypes)
      }
    }
  })
  
  # Generating HTML controls on fly in ui.R - Region End
  
  
  # Leaflet Map Generation - Start Region
  
  output$leafletMap <- renderLeaflet({
    
    varsOfInterest <- subset(outputType, type==input$tabSetPanel, variablesOfInterest)
    varNameToFilter <- subset(inputAttr, type==input$Attributes, varName)
    ipControlName <- subset(inputAttr, type==input$Attributes, inputControlName)
    varColor <- subset(inputAttr, type==input$Attributes, plotColor)

    leafletData <- listings %>% filter(eval(parse(text=paste(listingsDSName,varNameToFilter[[names(varNameToFilter)[1]]],sep="$"))) == eval(parse(text=paste("input",ipControlName[[names(ipControlName)[1]]],sep="$")))) %>%
                select(varsOfInterest[[names(varsOfInterest)[1]]])
    
    leaflet(data = leafletData) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = leafletData)
  })
  
  # Leaflet Map Generation - End Region
  
  
  
  output$listingplot <- renderPlot(
    {
      if(!is.null(input$Attributes) && input$Attributes != "") {
        varsOfInterest <- subset(outputType, type==input$tabSetPanel, variablesOfInterest)
        varNameToFilter <- subset(inputAttr, type==input$Attributes, varName)
        ipControlName <- subset(inputAttr, type==input$Attributes, inputControlName)
        varColor <- subset(inputAttr, type==input$Attributes, plotColor)
       
        newlistings <- subset(listings,
                              eval(parse(text=paste(listingsDSName,varNameToFilter[[names(varNameToFilter)[1]]],sep="$"))) == eval(parse(text=paste("input",ipControlName[[names(ipControlName)[1]]],sep="$"))),
                              select = varsOfInterest[[names(varsOfInterest)[1]]])
       
        NLdataframe <- data.frame(newlistings)
        bostonmap <- get_map(location = c(lon = -71.0589,
                                          lat = 42.3601),
                             zoom = 15,
                             maptype = "roadmap",
                             source = "google")
        displaymap <- ggmap(bostonmap,extent = "normal", maprange = TRUE) + geom_point(data= NLdataframe,
                                                                                       aes(x=longitude,
                                                                                           y=latitude),
                                                                                       colour = varColor[[names(varColor)[1]]])
        
      }
      
      if(input$Attributes == "Location")
      {
        displaymap
      }
      else if(input$Attributes == "Property Type")
      {
        displaymap
      }
      else if(input$Attributes == "Room Type")
      {
        displaymap
      }
      else if(input$Attributes == "Rate")
      {
        displaymap
      }
    }
  )
  output$summarytable <- renderPrint(
    {
      varsOfInterest <- subset(outputType, type==input$tabSetPanel, variablesOfInterest)
      varNameToFilter <- subset(inputAttr, type==input$Attributes, varName)
      ipControlName <- subset(inputAttr, type==input$Attributes, inputControlName)
      
      print(listings$id[1])
      
      if(!is.null(input$Attributes)) {
        newlistings <- subset(listings,
                              eval(parse(text=paste(listingsDSName,varNameToFilter[[names(varNameToFilter)]][1],sep="$"))) == eval(parse(text=paste("input",ipControlName[[names(ipControlName)]][1],sep="$"))),
                              select = varsOfInterest[[names(varsOfInterest)[1]]])


        summary(newlistings)
      }
    }
  )
  output$ReviewOut <- renderPlot(
    {
      if(input$Attributes == "Location")
      {

        reviewdf <- subset(listings,
                           listings$neighbourhood_cleansed == input$InputLocation,
                           select = c("summary"))
      }
      else if(input$Attributes == "Property Type")
      {
        reviewdf <- subset(listings,
                           listings$property_type == input$InputPropertyType,
                           select = c("summary"))
      }
      else if(input$Attributes == "Rate")
      {
        reviewdf <- subset(listings,
                           listings$transformed_price <= input$InputRate,
                           select = c("summary"))
      }
      
      wordscleanvs <- VectorSource(reviewdf)
      wordsclean <- SimpleCorpus(wordscleanvs, control = list(language = "en"))
      wordsclean <- tm_map(wordsclean, stripWhitespace)
      wordsclean <- tm_map(wordsclean, tolower)
      wordsclean <- tm_map(wordsclean, removeWords, stopwords("english"))
      wordsclean <- tm_map(wordsclean, removeWords, c("boston","kitchen","bed","apartment","view"))
      wordsclean <- tm_map(wordsclean, removeNumbers)
      wordsclean <- tm_map(wordsclean, removePunctuation)
      wordsclean <- tm_map(wordsclean, stemDocument)
      wordcloud(wordsclean, scale=c(5,0.5), max.words=100,min.freq=3, 
                random.order=FALSE, rot.per=0.35, 
                use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
    }
  )

}
)
