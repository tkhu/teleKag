# Project: AXA Kaggle Trip Driver Predicition
## Fuction: curveScore
## Title: Curve Score Visualizer Function
## Creator: Christopher Risi
## Create Date: January, 7th, 2015
## Modifier: Christopher Risi 
## Modified Date: January, 8th, 2015
###Version 0.4###

#################################
##############NOTES##############
#################################

###Make sure your working directory is the folder containing 
###all of the drivers folders that you wish to visualize.

## Version Revision Notes
###Version 0.3 Update Notes###
####Discovered a problem with using the end points to calculate the distance to the line
####should intead use the slope.  The reason is that a bug occurs when using end point.
####The bug is a result of the end point and therefore radius not being the max distance from origin.

###Version 0.4 Update Notes###
####Increased readability


# Function Start
curveScore <- function(driverNumber, displayCurves = FALSE)
  {
    driverNumber <- paste(driverNumber, sep="")
    
    setwd(driverNumber)
    
    i <- 1
    tripOne <- read.csv("1.csv")
    tripOne <- cbind(i,tripOne)
    
    alltripAggregate <- tripOne
  
    for(i in 2:200)
    	{
    		tripFileName <- paste(i, ".csv", sep="")
    
    		tempTrip <- read.csv(tripFileName)
    		tempTrip <- cbind(i, tempTrip)
    
    		alltripAggregate <- rbind(alltripAggregate, tempTrip)
    
    	}
  
  
    #Trip CurveScore Tracker
    curveTracker <- data.frame(tripNumber=1:200, radius=0, curveScore=0)
    
    #Radius Tracker
    radiusTracker <- data.frame(tripNumber=1:200, rx = 0, ry =0, slope=0)
    
    j = 1
    
    #Calculate Trip End Points
    for(j in 1:200)
    	{
    		subTrip <- alltripAggregate[alltripAggregate[,1]==j,] 
    	
    		tripEnd <- length(subTrip[,1])
    	
    		radiusTracker[j,2] = subTrip[tripEnd,2]
    		radiusTracker[j,3] = subTrip[tripEnd,3]
    		radiusTracker[j,4] = radiusTracker[j,3]/radiusTracker[j,2]
    	} #endJForLoop
    
    
    
    
    #Calculate Curve Scores
    for(k in 1:200)
    	{
    		subTripTwo <- alltripAggregate[alltripAggregate[,1]==k,] 
    		#Populate Radius
    		radius <- sqrt(((radiusTracker[k,2])^2) + ((radiusTracker[k,3])^2))
    		curveTracker[k,2] <- radius
    		sumDistances <- 0
    		#Populate CurveScore
    
    		
    		for(m in 1:length(subTripTwo[,1]))
    			{
      			sumDistances <- sumDistances + ((abs( (subTripTwo[m,2])*(radiusTracker[k,4]) -(subTripTwo[m,3])))/sqrt((radiusTracker[k,4]^2)+1))
      			if(m == length(subTripTwo[,1]))
      				{
        				curveTracker[k,3] <- sumDistances
        				sumDistances <- 0
      				} #endIf
    			} #end"m"ForLoop 		
    	}#end"k"ForLoop
    
    #Plotting Option
    if(displayCurves == TRUE)
    	{
    		par(mfrow=c(2,2))
    		plot(curveTracker[,3],curveTracker[,2], xlab="Curve Score", ylab="Radius (m)")
    		plot(log(curveTracker[,3]),curveTracker[,2], xlab="log(Curve Score)", ylab="Radius (m)")
    		plot(curveTracker[,3],log(curveTracker[,2]), xlab="Curve Score", ylab="log(Radius (m))")
    		plot(log(curveTracker[,3]),log(curveTracker[,2]), xlab="log(Curve Score)", ylab="log(Radius (m))")
    	}
    
    setwd("..")
    return(curveTracker)
  }#endOfFunction






