########Data CurveScore Methodology Example

###1. Load Curve Score Calculator (curveScore function)
###2. Load Path Plot Visualizer (read_trips function)
library(ggplot2)
library(gridExtra)
require(gridExtra)

ExampleCS1  <- curveScore(106)
ExampleRT <- read_trips("1080")

clrFrame <- data.frame(classification=0)
driverCurvesColour <- cbind(ExampleCS1, clrFrame)
head(driverCurvesColour)


####Log the Curve Score ####
##Create Curve Score Data Frame
driverCurvesColourLog <-driverCurvesColour
##for Loop to Populate Logs
for(j in 1:200)
	{
		driverCurvesColourLog[j,3] <- log(driverCurvesColour[j,3])
	}

CurveScoresRLog <- curveScores

for(j in 1:200)
{
  CurveScoresRLog[j,1] <- log(curveScores[j,1])
}

####Assign Classification Column on <<log(Curve Score) & Radius>> Values ####
for(i in 1:200)
{
	if(driverCurvesColourLog[i, 2] >= 15000)
		{
		driverCurvesColourLog[i,4] <- 1
		if((driverCurvesColourLog[i, 3] >= 11) & (driverCurvesColourLog[i, 3] <= 13 ))
			{
			driverCurvesColourLog[i,4] <- 3
			}
		
		}
	if(driverCurvesColourLog[i, 2] <= 15000)
		{
		driverCurvesColourLog[i,4] <- 2
		}

}


####Populate read_trips data with Classifications using driverCurvesColourLog
##Create Data Frame
clrFrame <- data.frame(classification=0)
driverPathsColour <- cbind(ExampleRT, clrFrame)

##Populate Classifications

for(k in 1:length(driverPathsColour[,3]))
	{
		
		driverPathsColour[k,5] <- driverCurvesColourLog[driverPathsColour[k,3],4]
		
	}


####Create Reduced Data Frames

driverCurvesLongTrips <- driverCurves[driverCurves[,2]>15000,]
dfReduced <-df[df[,3]==driverCurvesLongTrips[,1],]
driverCurvesSpecificTrips <- driverCurves[driverCurves[,1]==driverCurvesColourLog[,1],]


####Plotting####
require(gridExtra)
dataClass <- ggplot(driverCurvesColourLog, aes(curveScore, radius))
dataClassReduced <- ggplot(driverCurvesColourLogReduced, aes(curveScore, radius))
dataClassReduced <- ggplot(driverCurvesColourLogReduced, aes(curveScore, radius))


##Paths
#These look cool - exploreL8r##plot1 <- qplot(x,y,data=driverPathsColour,geom="path",group=classification,color=classification)
#These look cool - exploreL8r##plot2 <- qplot(x,y,data=driverPathsColourReduced,geom="path",group=classification,color=classification)

plot1 <- ggplot(driverPathsColour, aes(x=x, y=y, group=trip))+ geom_path(aes(colour = factor(classification)))

plot2 <- ggplot(driverPathsColourReduced, aes(x=x, y=y, group=trip))+ geom_path(aes(colour = factor(classification)))

##Classification of log(curves) vs Radius
plot3 <- dataClass + geom_point(aes(colour = factor(classification),shape = factor(classification)), size = 3) + scale_shape(solid = FALSE)
plot4 <- dataClassReduced + geom_point(aes(colour = factor(classification),shape = factor(classification)), size = 3) + scale_shape(solid = FALSE)

##display all
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, nrow=2)
grid.arrange(plot1, plot3, ncol=2)

##extra
####Assign Colour Column####
for(i in 1:200)
{
  if(driverCurvesColour[i, 2] >= 15000)
		{
		driverCurvesColour[i,4] <- 1
		}
	if(driverCurvesColour[i, 2] <= 15000)
		{
		driverCurvesColour[i,4] <- 2
		}

}

####Assign Colour Column on log(Curve Score) & Radius ####
for(i in 1:200)
{
	if(driverCurvesColourLog[i, 2] >= 15000)
		{
		driverCurvesColour[i,4] <- 1
		if((driverCurvesColour[i, 3] >= 2) & (driverCurvesColour[i, 3] <= 3 ))
			{
			driverCurvesColour[i,4] <- 3
			}
		
		}
	if(driverCurvesColour[i, 2] <= 15000)
		{
		driverCurvesColour[i,4] <- 2
		}

}



p <- ggplot(driverCurvesColourLog, aes(curveScore, radius))
p + geom_point(aes(colour = factor(colour),shape = factor(colour)))

driverCurvesColourLog <- tapply(driverCurvesColour, driverCurvesColour[,3], log)

driverCurvesColourLog <-driverCurvesColour
for(j in 1:200)
{
driverCurvesColourLog[j,3] <- log(driverCurvesColour[j,3])
}


## Simple Grid View
df <- read_trips("1")

qplot(x,y,data=df,geom="path",group=trip,color=trip)

#qplot(x,y,data=df,geom="path") + facet_wrap(~trip)

tripNum <- 4
TripDriverCharacteristics <- testC
ts.plot(ts(TripDriverCharacteristics[TripDriverCharacteristics[,1]==tripNum,7],),ts(TripDriverCharacteristics[TripDriverCharacteristics[,1]==tripNum,8]),col=1:2)
ts.plot(TripDriverCharacteristics[TripDriverCharacteristics[,1]==tripNum,7])
ts.plot(TripDriverCharacteristics[TripDriverCharacteristics[,1]==tripNum,8])
 max(TripDriverCharacteristics[,5])
