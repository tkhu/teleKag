# Project: AXA Kaggle Trip Driver Predicition
## Fuction: read_trips
## Title: Driver Trip Reader
## Creator: J.McLaren 
## Create Date: January  6, 2014
## Modifier: Christopher Risi 
## Modified Date: January, 12, 2015
## Version 0.2
## Version Revision Notes
### Version 0.2 Revisions
### Modified this function so that it will work on anyone's working driectory
### The working directory needs to contain at least the driver you are trying to use 
### the fucntion for, and the driver folder must contain at least one trip.

read_trips <- function(driver) 
{
  workDir <- getwd()
	print(paste("Driver:",driver))
	driver_files <- list.files(path = paste(workDir,driver,sep="/"))
	for (file in driver_files)
		{
		# if the merged dataset doesn't exist, create it
		if (!exists("dataset"))
			{
			dataset <- read.csv(paste(workDir,driver, file,sep="/"))

			dataset$trip <- strsplit(file,"[.]")[[1]][1]
			}

		# if the merged dataset does exist, append to it
		if (exists("dataset"))
			{
			temp_dataset <-read.csv(paste(workDir,driver,file,sep="/"))
			temp_dataset$trip <- strsplit(file,"[.]")[[1]][1]
			dataset<-rbind(dataset, temp_dataset)
			rm(temp_dataset)
			}
		}

dataset$driver <- driver
return(dataset)

}





