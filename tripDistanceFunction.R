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


tripDistance <-function(driverNumber, trips.df)
  {
    tripNumbers <- unique(trips.df$trip)
    tripNumbers <- as.numeric(tripNumbers)
    distance <- data.frame(trip=1:length(tripNumbers), driver=driverNumber, dist=0)
    for(i in 1:length(tripNumbers))
      {
        tripCoordinates <- trips.df[trips.df$trip==tripNumbers[i],1:2] 
        for(j in 2:length(tripCoordinates[,1]))
          {
            distance[i,3] <- distance[i,3] + sqrt(((tripCoordinates[j,1]-tripCoordinates[j-1,1])^2)+((tripCoordinates[j,2]-tripCoordinates[j-1,2])^2))
          }
      }
    
    return(distance)
  }