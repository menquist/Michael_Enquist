## google_maps_api.R
## Will Jenden
## November 16, 2016

# Details of the API can be found at:
# https://developers.google.com/maps/documentation/distance-matrix/intro#Introduction

# You can create a key from your console
# https://console.cloud.google.com/apis/api/distance_matrix_backend/overview
# You can create a key from your console
# https://console.cloud.google.com/apis/api/distance_matrix_backend/overview
setwd("~/Downloads/Translink/Cleaning & API Presentation/")

# Import libraries
library(RCurl) # https requests
library(RJSONIO) # manage JSON
library(dplyr)

# Google Maps Direction Matrix API key (unique to you)
key <- "AIzaSyC4PciZDUz6Y_7xV6rPB-x4unmfY1Oh_Ss"
unique.trips=readRDS("data/unique.trips.Rds") 
trips = readRDS("data/trips_subset.Rds")

# looking at bus stop address OperationDate %in% Schedule,
#keywords <- trips$Pattern[grep("*010", trips$Pattern)]
#streetname <- trips$StopName[grep("*", trips$StopName)]

#Schedule <- trips$OperationDate[grep("*", trips$OperationDate)] 
# filtering intergers and numerics 
#lookingatbusstop <- trips %>% 
#  filter( Pattern %in% keywords ) 
#str(lookingatbusstop)
unique.trips.line.10 <- trips %>% filter(Line=="010") %>% 
  distinct(Origin,Destination,.keep_all=T)%>%
  select(OriginLat,OriginLong,DestinationLat,DestinationLong,Hour)

# External file locations for easy updates
#unique.trips <- readRDS('data/unique.trips.Rds')


# FUNCTION TO BUILD URL FOR A QUERY
url <- function(origin, destination, API_key, return.call="json", units="metric"){
    # origin and destination is a vector of c(Lat, Long)
    # example Distance Matrix API call:
    # https://maps.googleapis.com/maps/api/distancematrix/json?units=metric&origins=49.25764,-123.1755&destinations=49.25758,-123.1718&key=YOURAPIKEY

    root <- "https://maps.google.com/maps/api/distancematrix/"
    u <- paste0(root,
        return.call,
        "?units=", units,
        "&origins=", origin[1], ",", origin[2],
        "&destinations=", destination[1], ",", destination[2],
        "&key=", API_key)

    return(URLencode(u))
}

# FUCNTION TO QUERY API AND STORE DISTANCE AND DURATION
queryAPI <- function(unique_trip, API_key, verbose=FALSE){
    # function takes a single observation as a dataframe with the origin and
    # destination, queries the API and returns result

    # code required if passing in row of data.frame
    origin = as.vector(select(unique_trip, OriginLat, OriginLong))
    destination = as.vector(select(unique_trip, DestinationLat, DestinationLong))

    u <- url(origin, destination, API_key)

    if(verbose){print(u)}

    d.json <- getURL(u)
    d.list <- fromJSON(d.json, simplifyWithNames = FALSE)

    if(verbose){print(d.list)}

    if(d.list$status=="OK"){
        if(d.list$rows[[1]]$elements[[1]]$status=="OK"){
            # distance in meters, duration transformed from seconds to minutes
            dist <- as.numeric(d.list$rows[[1]]$elements[[1]]$distance[["value"]])
            dur <- as.numeric(d.list$rows[[1]]$elements[[1]]$duration[["value"]])/60
            oA <- d.list$origin_addresses
            dA <- d.list$destination_addresses
            Quality <- "OK"
        } else {
            dist <- NA
            dur <- NA
            oA <- NA
            dA <- NA
            Quality <- d.list$rows[[1]]$elements[[1]]$status
        }
    } else if(d.list$status=="OVER_QUERY_LIMIT") {
        dist <- NA
        dur <- NA
        oA <- NA
        dA <- NA
        Quality <- d.list$status
    } else {
        stop("Distance Matrix API request issue. Status: ", d.list$status)
    }

    return(list(Distance=dist, Duration=dur, StartAddress=oA, StopAddress=dA, DM.Quality<-Quality))
}


## TEST API CONNECTION ON A SMALL NUMBER OF OBSERVATIONS
testAPIConnection <- function(unique.trips, n=10, key){
    # create subset to test fetching info and add empty columns
    test <- sample_n(unique.trips, n)
    
    for(row in 1:nrow(test)) {
        result <- queryAPI(test[row,], key, verbose=TRUE)
        test$API.Distance[row] <- result[[1]]
        test$API.Duration[row] <- result[[2]]
        test$API.StartAddress[row] <- result[[3]]
        test$API.StopAddress[row] <- result[[4]]
        test$API.DM.Quality[row] <- result[[5]]
    }

    test <-mutate(test, DM.Quality=factor(DM.Quality))
    return(test)
}


###############################################################################
# RUN IT FOR REAL. Requires calling the function from the console so you
# don't accidentally burn through your request limit. Note there is a 2500
# request limit per day. Each additional 1000 requests costs $0.50.
###############################################################################
updateTrips <- function(trips, StartRow, NumRequests){

    EndRow <- StartRow + NumRequests-1

    # don't request more data than you have
    if(EndRow > nrow(trips)){EndRow<-nrow(trips)}

    for(row in StartRow:EndRow) {
        # print progress updates
        if(row==StartRow){
            print("Starting API Call:")
            print(" 0% Complete...")
        } else if(row==round(0.25*(EndRow-StartRow)+StartRow, 0)){
            print("25% Complete...")
        } else if(row==round(0.5*(EndRow-StartRow)+StartRow, 0)){
            print("50% Complete...")
        } else if(row==round(0.75*(EndRow-StartRow)+StartRow, 0)){
            print("75% Complete...")
        }

        # query API and store results in dataframe
        result <- queryAPI(trips[row,], key)

        trips$API.Distance[row] <- result[[1]]
        trips$API.Duration[row] <- result[[2]]
        trips$API.StartAddress[row] <- result[[3]]
        trips$API.StopAddress[row] <- result[[4]]
        trips$API.DM.Quality[row] <- result[[5]]
    }

    trips <-mutate(trips, DM.Quality=factor(DM.Quality))

    # final status update
    print("100% Complete...")
    print(paste0("Summary of status of all ", EndRow-StartRow + 1, " requests:"))
    print(table(trips$DM.Quality[StartRow:EndRow], exclude=NULL))

    return(trips)
}


# EXAMPLE CALL TO FUNCTION AND SAVE TO RDS. BE CAREFUL NOT TO OVERWRITE GOOD DATA
#unique.trips <- updateTrips(unique.trips, 2501, 5000)
# trip.distance.file <- "data/google_trip_distances.Rds"
# saveRDS(unique.trips, trip.distance.file)

source('google_maps_api.R')
results <- testAPIConnection(unique.trips,n=10,key)
