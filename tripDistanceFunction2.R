# Project: AXA Kaggle Trip Driver Predicition
## Fuction: tripDistance
## Title: Calculate Total Trip Distance
## Creator: Christopher Risi
## Create Date: January, 16th, 2015
## Modifier:
## Modified Date:
## Version 0.1

#################################
##############NOTES##############
#################################
###This function will return the total trip distance for all trips for a particular driver

##Algorithm
### 1. Get single trip subset.
### 2. Loop through each second of trip and add euclidian distance to trip total until end of trip.
### 3. Last Trip? No go to step 1. Next trip.
### 4. Return a data frame with driver#, trip#, & Total Distance

###Version Notes###


tripDistance2 <-function(driverNumber, trips.df)
  {
    tripNumbers <- unique(trips.df$trip)
    tripNumbers <- as.numeric(tripNumbers)
    distance <- data.frame(trip=tripNumbers, driver=driverNumber, dist=0)
    for(i in 1:length(tripNumbers))
      {
        tripCoordinates <- trips.df[trips.df$trip==tripNumbers[i],1:2]

        xDist <- diff(tripCoordinates[,1], lag=1)
        yDist <- diff(tripCoordinates[,2], lag=1)

        xDist <- xDist^2
        yDist <- yDist^2

        eucDist <- xDist + yDist

        eucDist <- sqrt(eucDist)

        distance[i,3] <- sum(eucDist)

        xDist <- 0
        yDist <- 0
        eucDist <- 0
      }

    return(distance)
  }

