###############################################
##
## global.R - loading and defining variables for the global environment
##
###############################################


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

# Function to fread the files
extractData <- function(dsName, dsColumns) {
  fread(input=paste(paste(workingDir, dsName, sep = "/"),'.csv',sep=''), sep = ",", 
        header = "auto", select = dsColumns)
}

# Get current working directory
workingDir <- paste(getwd(),"Data", sep="/")

# Names of datasets
listingsDSName = "listings"
calendarDSName = "calendar"
reviewsDSName = "reviews"
mergedReviewsDSName = "mergedReviews"

colNamesListings = c("id", "name", "description", "summary", "neighbourhood_cleansed", 
                     "latitude", "longitude", "property_type", "room_type", "accommodates", 
                     "bathrooms", "bedrooms", "beds", "amenities", "transformed_price", 
                     "number_of_reviews", "review_scores_rating", "review_rating_transformed", 
                     "review_scores_location", "instant_bookable","listed_since_days",
                     "host_is_superhost","host_identity_verified","host_has_profile_pic",
                     "host_response_time","host_response_rate","host_acceptance_rate")

colNamesReviews = c("listing_id", "comments")


# Get the data
listings <- extractData(listingsDSName,colNamesListings)
reviews <- extractData(reviewsDSName,colNamesReviews)

# Merging Listings and Reviews datasets - Region Start
colnames(listings)[1] <- "listing_id"
mergedReviews <- merge(reviews, listings, by = "listing_id") #taking inner join
# Merging Listings and Reviews datasets - Region End


# Declaring & Initializing global variables - Region Start
outputType <- list(o1 = list(type='Map',reqDSTable=listingsDSName,variablesOfInterest=c("name","latitude","longitude","number_of_reviews","review_scores_rating","property_type","neighbourhood_cleansed","transformed_price","review_rating_transformed")),
                   o2 = list(type='Summary Statistics',reqDSTable=listingsDSName,variablesOfInterest=c("bedrooms","bathrooms","number_of_reviews","transformed_price","host_is_superhost","host_identity_verified","host_response_time","host_response_rate")),
                   o3 = list(type='Description',reqDSTable=listingsDSName,variablesOfInterest=c("description")),
                   o4 = list(type='Sentiments',reqDSTable=mergedReviewsDSName,variablesOfInterest=c("listing_id","name","property_type","neighbourhood_cleansed","comments"))
)

inputAttr <- list(a1 = list(type='Property Type',varName="property_type", inputControlName="InputPropertyType", plotColor="red"),
                  a2 = list(type='Location',varName="neighbourhood_cleansed", inputControlName="InputLocation", plotColor="blue"),
                  a3 = list(type='Room Type',varName="room_type", inputControlName="InputRoomType", plotColor="cyan"),
                  a4 = list(type='Rate',varName="transformed_price", inputControlName="InputRate", plotColor="green")
)

reqDataSetTableName=c("")
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


## --------------------------------------------------------------------------------------------------
## Code for Sentiment Analysis
## --------------------------------------------------------------------------------------------------

# Getting the top 10 neigbourhoods, based on listing - Start
top_neighbourhoods <- mergedReviews %>%
  select(listing_id,neighbourhood_cleansed,comments) %>%
  group_by(neighbourhood_cleansed) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(10)
# Getting the top 10 neigbourhoods, based on listing - End


calculateSentiments <- function(dataTable, filteredDT){
  
  #filtering reviews in these neigbourhoods - Start
  top_filteredReviews <- dataTable %>%
    filter(neighbourhood_cleansed %in% filteredDT$neighbourhood_cleansed)
  
  top_filteredReviews$comments <- as.character(top_filteredReviews$comments)
  #filtering reviews in these neigbourhoods - End
  
  #unnesting individual words for reviews - Start
  top_listings_words <- top_filteredReviews %>%
    select(listing_id, comments, neighbourhood_cleansed) %>%
    unnest_tokens(word, comments) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "^[a-z']+$"))
  #unnesting individual words for reviews - End
  
  #get word-sentiment lexicon - Start
  nrc <- sentiments %>%
    filter(lexicon == "nrc") %>%
    select(word, sentiment)
  #get word-sentiment lexicon - End
  
  #count total words in each neighbourhood - Start
  hood_tot_words <- top_listings_words %>%
    group_by(neighbourhood_cleansed) %>%
    mutate(total_words = n()) %>%
    ungroup() %>%
    distinct(listing_id, neighbourhood_cleansed, total_words)
  #count total words in each neighbourhood - End
  
  #count words for with each type of sentiment in each neighbourhood - Start
  by_hood_sentiment <- top_listings_words %>%
    inner_join(nrc, by = "word") %>%
    count(sentiment, listing_id) %>%
    ungroup() %>%
    inner_join(hood_tot_words) %>%
    group_by(neighbourhood_cleansed, sentiment, total_words) %>%
    summarize(words = sum(n)) %>%
    mutate(prop = round(words / total_words * 100, digits=1)) %>%
    ungroup()
  #count words for with each type of sentiment in each neighbourhood - End
  
  return(by_hood_sentiment)
}

# Generate Sentiments for the entire dataset - Region Start
by_nh_sentiment <- calculateSentiments(mergedReviews, top_neighbourhoods)
# Generate Sentiments for the entire dataset - Region End


