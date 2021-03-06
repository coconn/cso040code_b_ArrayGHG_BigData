# Code to analyze O2, temperaturea and moisture results from soil depth arrays
## Based on Process-arraysensorsdf-Sensor-Data-RCode


# packages
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(lubridate)

# define standard error function
ste <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# where to save outputs

sensordatapath = "C:/Users/jstar_000/Desktop/PC400 data/10-12-16/"
calibrationdatapath = "C:/Users/jstar_000/Desktop/PC400 data/"
outputdatapath = "C:/Users/jstar_000/Desktop/PC400 data/10-12-16/Depth Results/"

########################################################################
# BRING IN NEW DATA SHEETS

# what are the new temperature/moisture/O2 csv files to bring in?

CR1000_DepthTM <- read.csv(paste(sensordatapath,"CR1000_DatoutCS655.csv",sep=""),
                           stringsAsFactors=FALSE)
CR1000_oxygenEV <- read.csv(paste(sensordatapath,"CR1000_oxygenEVJune2015.csv",
                                  sep=""), stringsAsFactors=FALSE)



# where is the VWC data?
vwccols <- c(1,3,9,15,21,27,33,39,45,51,57,63,69,75,81,87,93,99,105)
dataend <- dim(CR1000_DepthTM)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesDepth <- c("TIMESTAMP","Sensor1","Sensor2","Sensor3","Sensor4","Sensor5",
                   "Sensor6","Sensor7","Sensor8","Sensor9","Sensor10","Sensor11",
                   "Sensor12","Sensor13","Sensor14","Sensor15",
                   "Sensor16","Sensor17","Sensor18") # these are arbitrary numbers, need to rename

# extract the data and give it useful col names
vwchourlyDepth <- CR1000_DepthTM[vwcrows, vwccols]
names(vwchourlyDepth) <- vwcnamesDepth

# replace 7999 values with NA
vwchourlyDepth[vwchourlyDepth==7999] <- NA
## because the datalogger uses 7999 for NA data

# make separate columns for date and time
vwchourlyDepth$TIMESTAMP2 <- parse_date_time(vwchourlyDepth$TIMESTAMP, orders="mdy HM") # as date
# get hour
vwchourlyDepth$Hour <- gsub( ".* ", "", vwchourlyDepth$TIMESTAMP) # as character
vwchourlyDepth$Hour2 <- hour(vwchourlyDepth$TIMESTAMP2) # as int
# get date
vwchourlyDepth$Date <- gsub( " .*$", "", vwchourlyDepth$TIMESTAMP) # as character
vwchourlyDepth$Date2 <- mdy(vwchourlyDepth$Date) # as date

## convert to long format

vwchourlylong <- gather(vwchourlyDepth, SensorID, VWC, Sensor1:Sensor18)
vwchourlylong$SensorID <- as.character(vwchourlylong$SensorID)
vwchourlylong$VWC <- as.numeric(vwchourlylong$VWC)

## get daily mean, sd, max, min
vwcdailylong <- ddply(vwchourlylong,.(Date2, SensorID),
                      summarize,
                      avgVWC=mean(VWC, na.rm = TRUE),
                      sdVWC=sd(VWC, na.rm = TRUE),
                      seVWC=ste(VWC),
                      maxVWC=max(VWC, na.rm = TRUE),
                      minVWC=min(VWC, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


## get Depth data

# where is the temp data?
tempcols <- c(1,5,11,17,23,29,35,41,47,53,59,65,71,77,83,89,95,101,107)
#vwccols  <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_DepthTM)[1]
temprows <- c(4:dataend)


# what are the variable names?
tempnamesDepth <- c("TIMESTAMP","Sensor1","Sensor2","Sensor3","Sensor4","Sensor5",
                    "Sensor6","Sensor7","Sensor8","Sensor9","Sensor10",
                    "Sensor11","Sensor12","Sensor13","Sensor14","Sensor15",
                    "Sensor16","Sensor17","Sensor18") #again arbitrary but should match the VWC data

# extract the data and give it useful col names
temphourlyDepth <- CR1000_DepthTM[temprows, tempcols]
names(temphourlyDepth) <- tempnamesDepth

# replace 7999 values with NA
temphourlyDepth[temphourlyDepth==7999] <- NA


# make separate columns for date and time
temphourlyDepth$TIMESTAMP2 <- parse_date_time(temphourlyDepth$TIMESTAMP, orders="mdy HM") # as date
# get hour
temphourlyDepth$Hour <- gsub( ".* ", "", temphourlyDepth$TIMESTAMP) # as character
temphourlyDepth$Hour2 <- hour(temphourlyDepth$TIMESTAMP2) # as int
# get date
temphourlyDepth$Date <- gsub( " .*$", "", temphourlyDepth$TIMESTAMP) # as character
temphourlyDepth$Date2 <- mdy(temphourlyDepth$Date) # as date

## convert to long format

temphourlylong <- gather(temphourlyDepth, SensorID, Temp, Sensor1:Sensor18)
temphourlylong$SensorID <- as.character(temphourlylong$SensorID)
temphourlylong$Temp <- as.numeric(temphourlylong$Temp)

## get daily mean, sd, max, min
tempdailylong <- ddply(temphourlylong,.(Date2, SensorID),
                       summarize,
                       avgTemp=mean(Temp, na.rm = TRUE),
                       sdTemp=sd(Temp, na.rm = TRUE),
                       seTemp=ste(Temp),
                       smaxTemp=max(Temp, na.rm = TRUE),
                       minTemp=min(Temp, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


# where is the O2 data?
O2cols <- c(1,331:345)
dataend <- dim(CR1000_oxygenEV)[1]
O2rows <- c(4:dataend)

# what are the variable names?
O2names <- c("TIMESTAMP",
             "Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06",
             "Sensor07","Sensor08","Sensor09","Sensor10","Sensor11","Sensor12",
             "Sensor13","Sensor14","Sensor15") ## Numbers DO NOT match up with the temp/moisture numbers-need to rename
#O2names <- as.character(CR1000_oxygenEV[1,O2cols])

# extract the data and give it useful col names
O2hourly <- CR1000_oxygenEV[O2rows, O2cols]
names(O2hourly) <- O2names

## make useful columns

# replace 7999 values with NA
O2hourly[O2hourly==7999] <- NA

# make separate columns for date and time
O2hourly$TIMESTAMP2 <- parse_date_time(O2hourly$TIMESTAMP, orders="mdy HM") # as date
# get hour
O2hourly$Hour <- gsub( ".* ", "", O2hourly$TIMESTAMP) # as character
O2hourly$Hour2 <- hour(O2hourly$TIMESTAMP2) # as int
# get date
O2hourly$Date <- gsub( " .*$", "", O2hourly$TIMESTAMP) # as character
O2hourly$Date2 <- mdy(O2hourly$Date) # as date

## convert to long format

O2hourlylong <- gather(O2hourly, SensorID, O2mV, Sensor01:Sensor15)
O2hourlylong$SensorID <- as.character(O2hourlylong$SensorID)
O2hourlylong$O2mV <- as.numeric(O2hourlylong$O2mV)

## convert from mV to percent using the calibration curves for each sensor

# bring in calibration curve info
O2cal <- read.csv(paste(calibrationdatapath,"O2sensorscalibrationdepth.csv",sep=""), stringsAsFactors=FALSE) #using average of all calibration data
# join to O2hourlylong
O2hourlylong <- full_join(O2hourlylong, O2cal)
# solve for percent O2
O2hourlylong$O2pct <- ((O2hourlylong$O2mV * O2hourlylong$Slope) - O2hourlylong$Intercept) * 100

## get daily mean, sd, max, min
O2dailylong <- ddply(O2hourlylong,.(Date2, SensorID),
                     summarize,
                     avgO2pct=mean(O2pct, na.rm = TRUE),
                     sdO2pct=sd(O2pct, na.rm = TRUE),
                     seO2pct=ste(O2pct),
                     smaxO2pct=max(O2pct, na.rm = TRUE),
                     minO2pct=min(O2pct, na.rm = TRUE)) # na.rm=T already in the function definition for ste()



########################################################################
# VWC + TEMP + O2 --> ONE DF

# add O2 dataframe into this once you've defined it

fulldaily1 <- full_join(tempdailylong, vwcdailylong)
fulldaily <- full_join(fulldaily1, O2dailylong) #when adding transect # etc this was called fulldaily2

#### modify this to ID sensors by depth and position later?
# put transect info in as a column near the front for easy reading
#definetransects <- read.csv(paste(calibrationdatapath,"definetransects.csv",sep=""), stringsAsFactors=FALSE)
#fulldaily3 <- full_join(fulldaily2, definetransects)
# dim(fulldaily3)[2] is where TransectID is; move to early column
#endcol <- dim(fulldaily3)[2]
#secondlast <- endcol-1
#fulldaily <- fulldaily3[,c(1:2,endcol,3:secondlast)]

# add TopoLocation
#defineTopoLocation <- read.csv(paste(calibrationdatapath,"defineTopoLocation.csv",sep=""), stringsAsFactors=FALSE)
#fulldaily4 <- full_join(fulldaily, defineTopoLocation)
# dim(fulldaily4)[2] is where TopoLocation is; move to early column
#endcol <- dim(fulldaily4)[2]
#secondlast <- endcol-1
#fulldaily <- fulldaily4[,c(1:2,endcol,3:secondlast)]
#fulldaily$TopoLocation <- as.factor(fulldaily$TopoLocation)


########################################################################
# DEFINE WHAT GETS ADDED TO THE RUNNING SPREADSHEET

# hourly wide for each
head(vwchourlyDepth)
head(temphourlyDepth)
head(O2hourly)

# daily wide for each
# go from long to wide using spread() from tidyr
# only do this for the average daily values, not the sd, se, min or max

vwcdailylongavg <- vwcdailylong[,c(1:3)]
vwcdailywideavg <- spread(vwcdailylongavg, SensorID, avgVWC)

tempdailylongavg <- tempdailylong[,c(1:3)]
tempdailywideavg <- spread(tempdailylongavg, SensorID, avgTemp)

O2dailylongavg <- O2dailylong[,c(1:3)]
O2dailywideavg <- spread(O2dailylongavg, SensorID, avgO2pct)

head(tempdailywideavg)
head(vwcdailywideavg)
head(O2dailywideavg)

# daily long combined dataset
head(fulldaily)


########################################################################
# SAVE AS CSVS

# hourly wide for each

# make ok to write to CSV
vwchourlyDepth$TIMESTAMP2 <- as.character(vwchourlyDepth$TIMESTAMP2)
vwchourlyDepth$Date2 <- as.character(vwchourlyDepth$Date2)
temphourlyDepth$TIMESTAMP2 <- as.character(temphourlyDepth$TIMESTAMP2)
temphourlyDepth$Date2 <- as.character(temphourlyDepth$Date2)
O2hourlylong$TIMESTAMP2 <- as.character(O2hourlylong$TIMESTAMP2)
O2hourlylong$Date2 <- as.character(O2hourlylong$Date2)

# write to csv
write.csv(vwchourlyDepth, file=paste(outputdatapath, "vwchourly.csv", sep = ""), row.names=FALSE)
write.csv(temphourlyDepth, file=paste(outputdatapath, "temphourly.csv", sep = ""), row.names=FALSE)
write.csv(O2hourlylong, file=paste(outputdatapath, "O2hourly.csv", sep = ""), row.names=FALSE)

# daily wide for each

# make ok to write to CSV
vwcdailywideavg$Date2 <- as.character(vwcdailywideavg$Date2)
tempdailywideavg$Date2 <- as.character(tempdailywideavg$Date2)
O2dailywideavg$Date2 <- as.character(O2dailywideavg$Date2)

# write to csv
write.csv(vwcdailywideavg, file=paste(outputdatapath, "vwcdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(tempdailywideavg, file=paste(outputdatapath, "tempdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(O2dailywideavg, file=paste(outputdatapath, "O2dailywideavg.csv", sep = ""), row.names=FALSE)

# daily long combined dataset

# make ok to write to CSV
fulldaily$Date2 <- as.character(fulldaily$Date2)

# write to csv
write.csv(fulldaily, file=paste(outputdatapath, "fulldaily.csv", sep = ""), row.names=FALSE)