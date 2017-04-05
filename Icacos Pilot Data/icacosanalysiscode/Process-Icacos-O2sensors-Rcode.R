# Process-Icacos-O2sensors-Rcode.R
# Code to analyze O2 from pilot Icacos installation (4 sensors)
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

sensordatapath = "C:/Users/jstar_000/Desktop/PC400 data/Icacos/"
calibrationdatapath = "C:/Users/jstar_000/Desktop/PC400 data/Icacos/"
outputdatapath = "C:/Users/jstar_000/Desktop/PC400 data/Icacos/Results/"

# bring in data
CR1000_oxygenEV <- read.csv(paste(sensordatapath,"CR1000_Oxygen.csv",
                                  sep=""), stringsAsFactors=FALSE)

# O2: HOURLY --> DAILY

# get O2 datalogger data
# AI-AX = sensor 1-16 (col 35-50), FG-FV = sensor 17-32 (col 163-178), JE-JG = sensor 33-35 (col 265-267)
# note that this data is all in mV until we convert it later

# where is the O2 data?
O2cols <- c(1,5:8)
dataend <- dim(CR1000_oxygenEV)[1]
O2rows <- c(4:dataend)

# what are the variable names?
O2names <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04")

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

O2hourlylong <- gather(O2hourly, SensorID, O2mV, Sensor01:Sensor04)
O2hourlylong$SensorID <- as.character(O2hourlylong$SensorID)
O2hourlylong$O2mV <- as.numeric(O2hourlylong$O2mV)

## convert from mV to percent using the calibration curves for each sensor

# bring in calibration curve info
O2cal <- read.csv(paste(calibrationdatapath,"O2sensorscalibration.csv",sep=""), stringsAsFactors=FALSE) 

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


# daily wide for each
# go from long to wide using spread() from tidyr
# only do this for the average daily values, not the sd, se, min or max
O2dailylongavg <- O2dailylong[,c(1:3)]
O2dailywideavg <- spread(O2dailylongavg, SensorID, avgO2pct)


########################################################################
# SAVE AS CSVS

# hourly wide

# write to csv
write.csv(O2hourlylong, file=paste(outputdatapath, "IcacosO2hourly.csv", sep = ""), row.names=FALSE)


# write to csv
write.csv(O2dailywideavg, file=paste(outputdatapath, "IcacosO2dailywideavg.csv", sep = ""), row.names=FALSE)


# diagnostic graph
ggplot(O2hourlylong[],aes(x=TIMESTAMP2,y=O2pct,color=SensorID)) + 
  geom_point() +
  labs(x="Time",y="O2 concentration") 
