# Project: AXA Kaggle Trip Driver Predicition
## Fuction: tripDvrCharacter
## Title: Calculate Trip Characteristics Distance Speed Acceleration by Second
## Creator: Christopher Risi
## Create Date: January, 23rd, 2015
## Modifier:
## Modified Date:
## Version 0.1

#################################
##############NOTES##############
#################################
###This function will return the total trip distance for all trips for a particular driver

##Algorithm

###Version Notes###


tripDvrCharacter <-function(driverNumber, trips.df)
  {
    tripNumbers <- unique(trips.df$trip)
    tripNumbers <- as.numeric(tripNumbers)
    totalObservations <- length(trips.df[,1])
    TripDriverCharacteristics <- data.frame(trip=rep(0,totalObservations), x=0, y=0, xSpeed=0, ySpeed=0, Distance.Meter=0, Speed.Meter.Sec=0, Acceleration.Meter.Sec.Sec=0)
    j = 1

    for(i in 1:length(tripNumbers))
    {
        tripCoordinates <- trips.df[trips.df$trip==tripNumbers[i],1:2]

        xSpeed <- c(0,diff(tripCoordinates[,1], lag=1))
        ySpeed <- c(0,diff(tripCoordinates[,2], lag=1))

        xSqr <- xSpeed^2
        ySqr <- ySpeed^2

        eucDist <- xSqr + ySqr
        eucDist <- sqrt(eucDist)

        Speed.Meter.Sec <- eucDist

        Distance.Meter <- cumsum(eucDist)

        Acceleration.Meter.Sec.Sec <- diff(Speed.Meter.Sec, lag=1)
        Acceleration.Meter.Sec.Sec <- c(0, Acceleration.Meter.Sec.Sec)

        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),1] <- i
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),2] <- tripCoordinates[,1]
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),3] <- tripCoordinates[,2]
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),4] <- xSpeed
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),5] <- ySpeed
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),6] <- Distance.Meter
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),7] <- Speed.Meter.Sec
        TripDriverCharacteristics[j:(j+length(tripCoordinates[,1])-1),8] <- Acceleration.Meter.Sec.Sec

        j <- j + length(tripCoordinates[,1])

        xSpeed <- 0
        ySpeed <- 0
        eucDist <- 0
        xSqr <- 0
        ySqr <- 0
        Speed.Meter.Sec <- 0
        Distance.Meter <- 0
        Acceleration.Meter.Sec.Sec <- 0
    }


    return(TripDriverCharacteristics)
  }
