
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
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
library(janeaustenr)
library(tidytext)
library(stringr)
library(treemap)



## ------------------------------------------------------------------------------------------------------
## shinyServer function begins here ->
## ------------------------------------------------------------------------------------------------------

shinyServer(function(input, output) {

## ------------------------------------------------------------------------------------------------------
## 
## Code for Visualizations Navigation Bar below ->
##
## ------------------------------------------------------------------------------------------------------
 
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
    }
  })
  # Generating HTML controls on fly in ui.R - Region End
  
  
  # Filtering listings rown based on selection in the sidebar panel - Region Start
  extractSelectionData <- reactive({
    if(!is.null(input$Attributes)){
      reqDataSetTableName <- subset(outputType, type==input$tabSetPanel, reqDSTable)
      varsOfInterest <- subset(outputType, type==input$tabSetPanel, variablesOfInterest)
      varNameToFilter <- subset(inputAttr, type==input$Attributes, varName)
      ipControlName <- subset(inputAttr, type==input$Attributes, inputControlName)
      varColor <- subset(inputAttr, type==input$Attributes, plotColor)
      
      if(input$Attributes == "Rate")
      {
        subset(eval(parse(text=reqDataSetTableName[[names(reqDataSetTableName)]][1])),
               eval(parse(text=paste(reqDataSetTableName[[names(reqDataSetTableName)]][1],varNameToFilter[[names(varNameToFilter)]][1],sep="$"))) 
               <= eval(parse(text=paste("input",ipControlName[[names(ipControlName)]][1],sep="$"))),
               select = varsOfInterest[[names(varsOfInterest)[1]]])
      }
      else
      {
        subset(eval(parse(text=reqDataSetTableName[[names(reqDataSetTableName)]][1])),
               eval(parse(text=paste(reqDataSetTableName[[names(reqDataSetTableName)]][1],varNameToFilter[[names(varNameToFilter)]][1],sep="$"))) 
               == eval(parse(text=paste("input",ipControlName[[names(ipControlName)]][1],sep="$"))),
               select = varsOfInterest[[names(varsOfInterest)[1]]])
      }
    }
  })
  # Filtering listings rown based on selection in the sidebar panel - Region End
  
  
  # Leaflet Map Generation - Region Start
  output$leafletMap <- renderLeaflet({
    
    if(nrow(listings) > 0 && !is.null(input$Attributes)) {
      
      # Grab the filtered listings data based on current selection
      leafletData <- extractSelectionData()
      
      if(nrow(leafletData) > 0 && !is.null(leafletData$latitude)) {
        
        # Create our colors with a categorical color function
        color <- colorFactor(topo.colors(7), leafletData$neighbourhood_cleansed)
        
        leaflet(data = leafletData) %>%
          setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>%
          addProviderTiles(
                           providers$CartoDB.Positron,
                           options = providerTileOptions(noWrap = TRUE)
                          ) %>%
          #addGraticule()
          addMarkers(
                     lat =~leafletData$latitude,
                     lng =~leafletData$longitude,
                     clusterOptions = markerClusterOptions(),
                     popup = paste(
                       "<b>", as.character(leafletData$name), "<br/></b>",
                       "Price: $", as.numeric(leafletData$transformed_price), "<br/>",
                       "<i> Rating: ", as.numeric(leafletData$review_rating_transformed), "</i>"
                      ),
                     options = markerOptions(riseOnHover = TRUE)
                     )
      }
    }
  })
  # Leaflet Map Generation - Region End
  
  
 
  
  # Box plot to see price variation based on selection - Region Start
  output$plot1 <- renderPlot(
    {
      if(!is.null(input$Attributes)) {
        
        # Grab the filtered listings data based on current selection
        plot1Data <- extractSelectionData()
        
        if(nrow(plot1Data) > 0)
        {
          req(plot1Data$transformed_price)
            boxplot(plot1Data$transformed_price, ylim = c(min(plot1Data$transformed_price, na.rm = FALSE),max(plot1Data$transformed_price, na.rm = FALSE)), main = "Variation In Price", xlab = "Daily Room Rate", ylab = "Price Range", na.action = NULL)
        }
      }
    }
  )
  #  Box plot to see price variation based on selection - Region End
  
  
  # Histogram plot to see variation in number of bedrooms based on selection - Region Start
  output$plot2 <- renderPlot(
    {
      if(!is.null(input$Attributes)) {
        
        # Grab the filtered listings data based on current selection
        plot2Data <- extractSelectionData()
        counts <- table(factor(plot2Data$host_is_superhost))
        
        if(nrow(plot2Data) > 0 && !is.null(counts))
          barplot(counts, xlim = NULL, ylim = c(0,nrow(plot2Data)), main = "Proportion Of Super Hosts", xlab = "Number of Super Hosts", ylab = "Count")
      }
    }
  )
  # Histogram plot to see variation in number of bedrooms based on selection - Region End
  
  
  # Histogram plot to see variation in number of bathrooms based on selection - Region Start
  output$plot3 <- renderPlot(
    {
      if(!is.null(input$Attributes)) {
        
        # Grab the filtered listings data based on current selection
        plot3Data <- extractSelectionData()
        counts <- table(factor(plot3Data$host_identity_verified))
        
        if(nrow(plot3Data) > 0 && !is.null(counts))
          barplot(counts, xlim = NULL, ylim = c(0,nrow(plot3Data)), main="Proportion Of Verified Hosts", xlab = "Number of Identity Verified Hosts", ylab="Count")
        
      }
    }
  )
  # Histogram plot to see variation in number of bathrooms based on selection - Region End
  
  
  # Histogram plot to see variation in number of reviews based on selection - Region Start
  output$plot4 <- renderPlot(
    {
      if(!is.null(input$Attributes)) {
        
        # Grab the filtered listings data based on current selection
        plot4Data <- extractSelectionData()
        
        if(nrow(plot4Data) > 0)
        {
          req(plot4Data$number_of_reviews)
            hist(as.numeric(plot4Data$number_of_reviews), ylim = c(0,max(plot4Data$number_of_reviews, na.rm = FALSE)), main="Variation In Number Of Reviews", xlab = "Number of Reviews", ylab="Count")
        }
      }
    }
  )
  # Histogram plot to see variation in number of reviews based on selection - Region End
  
  
  # Word Cloud Generation - Region Start
  output$ReviewOut <- renderPlot(
    {
      # Grab the filtered listings data based on current selection
      reviewdf <- extractSelectionData()
      
      if(!is.null(reviewdf)) {
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
    }
  )
  # Word Cloud Generation - Region End
  
  
  # Sentiment Analysis to see vibes of a neighbourhood for entire dataset - Region Start
  output$sentimentPlot <- renderPlot({
    
    if(!is.null(by_nh_sentiment)) {
      treemap(by_nh_sentiment, index=c("neighbourhood_cleansed","sentiment"), vSize="prop", vColor="neighbourhood_cleansed", 
              type="index", title="Sentiment Analysis in Neighbourhood", vertex.size = 12, palette=brewer.pal(n=8, "Set3"), 
              fontsize.title = 20, fontsize.labels = 15, fontsize.legend = 16, fontcolor.labels = "Black", fontfamily.title = "sans", 
              fontfamily.labels = "sans", fontfamily.legend = "sans")
    }
  })
  # Sentiment Analysis to see vibes of a neighbourhood for entire dataset - Region End
  
  
  # Dynamic Sentiment Analysis to see vibes of a neighbourhood based on selection - Region Start
  output$dynamicSentimentPlot <- renderPlot({
    
    if(!is.null(input$Attributes)){
    
      sentimentData <- extractSelectionData()
      by_nh_selection_sentiment <- calculateSentiments(mergedReviews, sentimentData)
      
      if(!is.null(by_nh_selection_sentiment)) {
        req(by_nh_selection_sentiment$sentiment)
        ggplot(data=by_nh_selection_sentiment, aes(x=neighbourhood_cleansed, y=prop, fill=neighbourhood_cleansed)) + geom_bar(stat="identity") + facet_wrap(~ sentiment) +
          labs(title="Variation In Vibes Of Neighbourhoods Based On Selection Variables:",
               x="Sentiments", y="Proportion Index") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
      }
    }
  })
  # Dynamic Sentiment Analysis to see vibes of a neighbourhood based on selection - Region End
  
  
## ---------------------------------------------------------------------------------------------------
## 
## Code for 'Influential Predictros' Navigation Bar below ->
## This is the code for Regression explanatory model
##
## ---------------------------------------------------------------------------------------------------
  
  # ANOVA statistics based on Chi-Square test - Region End
  output$Regression <- renderPrint({
    
    predictors <- listings %>% select(review_rating_transformed,listed_since_days,host_is_superhost,
                                      transformed_price,host_identity_verified,host_has_profile_pic,
                                      number_of_reviews,host_response_time,
                                      host_response_rate,host_acceptance_rate,property_type,
                                      room_type,neighbourhood_cleansed)
    
    model <- glm(review_rating_transformed ~.,data=predictors)
    anova(model,test="Chisq")
    #stargazer(model,type='text')
  }
  )
  # ANOVA statistics based on Chi-Square test - Region End
  
}
)

shinyApp(shinyUI,shinyServer)
