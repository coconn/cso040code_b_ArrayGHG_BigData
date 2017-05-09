# Code to analyze O2, temperaturea and moisture results from soil depth arrays
## Based on Process-arraysensorsdf-Sensor-Data-RCode


# packages
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(lubridate)
library(ggplot2)

# define standard error function
ste <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# where to save outputs

sensordatapath = "~/Desktop/Datalogger_downloads/5-9-17/"
calibrationdatapath = "~/Desktop/Datalogger_downloads/Calibration files/"
outputdatapath = "~/Desktop/Datalogger_downloads/5-9-17/Depth results/"

########################################################################
# BRING IN NEW DATA SHEETS

# what are the new temperature/moisture/O2 csv files to bring in?

CR1000_DepthTM <- read.csv(paste(sensordatapath,"CR1000_DatoutCS655.csv",sep=""),
                           stringsAsFactors=FALSE)
CR1000_oxygenEV <- read.csv(paste(sensordatapath,"CR1000_oxygenEVJune2015.csv",
                                  sep=""), stringsAsFactors=FALSE)



# where is the VWC data?
vwccols <- c(1,3,9,15,21,27,33,39,45,51,57,63,69,75,81,219,221,223,225)
dataend <- dim(CR1000_DepthTM)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesDepth <- c("TIMESTAMP","VWC1","VWC2","VWC3","VWC4","VWC5",
                   "VWC6","VWC7","VWC8","VWC9","VWC10","VWC11",
                   "VWC12","VWC13","VWC14","VWC15",
                   "VWC16","VWC17","VWC18") # these are arbitrary numbers, need to rename

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

vwchourlylong <- gather(vwchourlyDepth, SensorID, VWC, VWC1:VWC18)
vwchourlylong$SensorID <- as.character(vwchourlylong$SensorID)
vwchourlylong$VWC <- as.numeric(vwchourlylong$VWC)

## add real sensor IDs

VWCIDs <- read.csv(paste(calibrationdatapath,"VWCIDmatching.csv",
                         sep=""), stringsAsFactors=FALSE)
vwchourlylongcor <- full_join(vwchourlylong,VWCIDs)

## get daily mean, sd, max, min
vwcdailylong <- ddply(vwchourlylongcor,.(Date2, SensorID, 
                                         RealID, TopoLocation,Depth),
                      summarize,
                      avgVWC=mean(VWC, na.rm = TRUE),
                      sdVWC=sd(VWC, na.rm = TRUE),
                      seVWC=ste(VWC),
                      maxVWC=max(VWC, na.rm = TRUE),
                      minVWC=min(VWC, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


## get Depth data

# where is the temp data?
tempcols <- c(1,5,11,17,23,29,35,41,47,53,59,65,71,77,83) #no temp data from C616?
#vwccols  <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_DepthTM)[1]
temprows <- c(4:dataend)


# what are the variable names?
tempnamesDepth <- c("TIMESTAMP","VWC1","VWC2","VWC3","VWC4","VWC5",
                    "VWC6","VWC7","VWC8","VWC9","VWC10",
                    "VWC11","VWC12","VWC13","VWC14") #again arbitrary but should match the VWC data

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

temphourlylong <- gather(temphourlyDepth, SensorID, Temp, VWC1:VWC14)
temphourlylong$SensorID <- as.character(temphourlylong$SensorID)
temphourlylong$Temp <- as.numeric(temphourlylong$Temp)

# add real IDs

temphourlylongcor <- full_join(temphourlylong,VWCIDs)

## get daily mean, sd, max, min
tempdailylong <- ddply(temphourlylongcor,.(Date2, SensorID, 
                                           RealID, TopoLocation,Depth),
                       summarize,
                       avgTemp=mean(Temp, na.rm = TRUE),
                       sdTemp=sd(Temp, na.rm = TRUE),
                       seTemp=ste(Temp),
                       smaxTemp=max(Temp, na.rm = TRUE),
                       minTemp=min(Temp, na.rm = TRUE)) # na.rm=T already in the function definition for ste()


# where is the O2 data?
O2cols <- c(1,331:346,272:273)
dataend <- dim(CR1000_oxygenEV)[1]
O2rows <- c(4:dataend)

# what are the variable names?
O2names <- c("TIMESTAMP",
             "Ox01","Ox02","Ox03","Ox04","Ox05","Ox06",
             "Ox07","Ox08","Ox09","Ox10","Ox11","Ox12",
             "Ox13","Ox14","Ox15","Ox16","Ox17","Ox18") 

# New sensors wired on May4 2017 and installed May8 2017:
    #Ox16= slope15
    #Ox17= slope30
    #Ox18= slope45
# Also corrected wiring of Ox12 (ridge 90cm) on May4 2017
# Also wired in new sensors for Ox 10 (valley 75) and Ox05 (ridge 30) on May4 2017;
    #installed in ground on May8 2017

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

O2hourlylong <- gather(O2hourly, SensorID, O2mV, Ox01:Ox18)
O2hourlylong$SensorID <- as.character(O2hourlylong$SensorID)
O2hourlylong$O2mV <- as.numeric(O2hourlylong$O2mV)

## convert from mV to percent using the calibration curves for each sensor

# bring in calibration curve info
O2cal <- read.csv(paste(calibrationdatapath,"O2sensorscalibrationdepth.csv",sep=""), stringsAsFactors=FALSE) #using average of all calibration data
# join to O2hourlylong
O2hourlylong <- full_join(O2hourlylong, O2cal)
# solve for percent O2
O2hourlylong$O2pct <- ((O2hourlylong$O2mV * O2hourlylong$Slope) - O2hourlylong$Intercept) * 100

## add real sensor IDs

OxIDs <- read.csv(paste(calibrationdatapath,"OxIDmatching.csv",
                         sep=""), stringsAsFactors=FALSE)
O2hourlylongcor <- full_join(O2hourlylong,OxIDs)

## get daily mean, sd, max, min
O2dailylong <- ddply(O2hourlylongcor,.(Date2, SensorID, 
                                       RealID, TopoLocation,Depth),
                     summarize,
                     avgO2pct=mean(O2pct, na.rm = TRUE),
                     sdO2pct=sd(O2pct, na.rm = TRUE),
                     seO2pct=ste(O2pct),
                     smaxO2pct=max(O2pct, na.rm = TRUE),
                     minO2pct=min(O2pct, na.rm = TRUE)) # na.rm=T already in the function definition for ste()



########################################################################
# VWC + TEMP + O2 --> ONE DF

# add O2 dataframe into this once you've defined it

fulldaily1 <- full_join(tempdailylong, vwcdailylong, 
                        by=c("Date2","RealID","SensorID","TopoLocation","Depth"))
fulldaily <- full_join(fulldaily1, O2dailylong, by=c("Date2","RealID","TopoLocation","Depth")) #when adding transect # etc this was called fulldaily2

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

vwcdailylongavg <- vwcdailylong[,c(1,3,6)]
vwcdailywideavg <- spread(vwcdailylongavg, RealID, avgVWC)

tempdailylongavg <- tempdailylong[,c(1,3,6)]
tempdailywideavg <- spread(tempdailylongavg, RealID, avgTemp)

O2dailylongavg <- O2dailylong[,c(1,3,6)]
O2dailywideavg <- spread(O2dailylongavg, RealID, avgO2pct)

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

# make basic graphs

figuredata <- fulldaily[!is.na(fulldaily$TopoLocation),]

ggplot(figuredata,aes(x=as.Date(Date2),y=avgO2pct,color=factor(Depth))) + 
  geom_point() +
  scale_x_date(limits=as.Date(c("2017-01-01","2017-07-01"))) +
  labs(x="Date",y="O2 concentration") +
  facet_grid(TopoLocation~.)
ggsave("DepthO2.jpg",path=outputdatapath)

ggplot(figuredata,aes(x=as.Date(Date2),y=avgTemp,color=factor(Depth))) +
  geom_point() +
  scale_x_date(limits=as.Date(c("2017-01-01","2017-07-01"))) +
  labs(x="Date",y="Soil Temp") +
  facet_grid(TopoLocation~.)
ggsave("DepthTemp.jpg",path=outputdatapath)

ggplot(figuredata,aes(x=as.Date(Date2),y=avgVWC,color=factor(Depth))) + 
  geom_point() +
  scale_x_date(limits=as.Date(c("2017-01-01","2017-07-01"))) +
  labs(x="Date",y="Volumetric Water Content") +
  facet_grid(TopoLocation~.)
ggsave("DepthVWC.jpg",path=outputdatapath)