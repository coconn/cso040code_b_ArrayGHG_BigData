# Process-arraysensorsdf-Sensor-Data-Rcode.R
# 
# processing raw data files from the PR array sensors
#
# code heavily indebted to: pitTDR_CR1000_PL_02042014.R
# AUTHORS: Paulo Brando, Paul Lefebvre, Marcia Macedo
#
# O2 - redox - GHG project
# CS O'Connell, UCB, Silver Lab

# output products:
# 


########################################################################
# GET READY TO BRING IN DATA

library(ggplot2)
library(gridExtra)
library(scales)
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(chron)
library(lubridate)
library(lattice)
library(reshape2)

# define standard error function
ste <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

# where to save outputs

# for PR data computer
sensordatapath = "~/Desktop/Datalogger_downloads/7-18-17/"
calibrationdatapath = "~/Desktop/Datalogger_downloads/Calibration files/"
outputdatapath = "~/Desktop/Datalogger_downloads/7-18-17/Surface results/"

# for christine
#sensordatapath = "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceOutOfDate/OldestCSOHas/renamedtomatchcode/"
#calibrationdatapath = "~/Documents/GITHUB/cso040code_ArrayGHG/ArrayGHG-Data-Raw/Sensor-data-Ryan-practice/"
#outputdatapath = "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceOutOfDate/OldestCSOHas/renamedtomatchcode/"

########################################################################
# BRING IN NEW DATA SHEETS

# Import moisture/temperature, oxygen and redox
CR1000_EIsoilT1 <- read.csv(paste(sensordatapath,"CR1000_EIsoilT1June2015.csv",
                                  sep=""), stringsAsFactors=FALSE)
CR1000_EIsoilT2 <- read.csv(paste(sensordatapath,"CR1000_EIsoilT2June2015.csv",
                                  sep=""), stringsAsFactors=FALSE)
CR1000_oxygenEV <- read.csv(paste(sensordatapath,"CR1000_oxygenEVJune2015.csv",
                                  sep=""), stringsAsFactors=FALSE)
CR1000_redoxR   <- read.csv(paste(sensordatapath,"SN31734-S_R_Table1.csv",
                                  sep=""), stringsAsFactors=FALSE)
CR1000_redoxV   <- read.csv(paste(sensordatapath,"SN31737_V_Table1.csv",
                                  sep=""), stringsAsFactors=FALSE)
# remember to change files to .csv and delete extra columns at end
########################################################################
# INFO ABOUT THE DATA

## temperature and moisture info is in the same files:
# T1 .csv files is through sensor 19 and T2 is the sensors from 20-35
# vol water content columns are labeled as: VWC_Avg, VWC_2_Avg, VWC_3_Avg, etc. in row 2
# VWC is in units of m^3/m^3
# temperature columns are labeled as: T_Avg, T_2_Avg, T_3_Avg, etc. also in row 2
# VWC is in units of Deg C
# we want the columns that say "Avg" in row 4, not those that say "Smp" in row 4

## oxygen info is in its own file:
# in excel, the relevant columns for O2 data are
# AI-AX = sensor 1-16, FG-FV = sensor 17-32, JE-JG = sensor 33-35
# see the calibration curve stuff in the hourly tab in the excel file

## redox data is in two files, "r" and "v" (ridge and valley?)
# relevant columns are named "SEVolt_Avg" or "SEVolt_#_Avg"
# for "v" file, begin with column C (3), then every 4 (3,7,11,15) (sensors 1 to 4)
# for "r" file, begin with column C (3), then every 4 (3,7,11,15,19,23,27,31) (sensors 1 to 8)

########################################################################
# VWC: HOURLY --> DAILY

## get T1 data

# where is the Volumentric Water Content (VWC) data?
vwccols <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39)
dataend <- dim(CR1000_EIsoilT1)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesT1 <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                "Sensor17","Sensor18","Sensor19")
#vwcnamesT1 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
vwchourlyT1 <- CR1000_EIsoilT1[vwcrows, vwccols]
names(vwchourlyT1) <- vwcnamesT1

## get T2 data

# where is the VWC data?
vwccols <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_EIsoilT2)[1]
vwcrows <- c(4:dataend)

# what are the variable names?
vwcnamesT2 <- c("TIMESTAMP","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
#vwcnamesT2 <- as.character(CR1000_EIsoilT1[1,vwccols])


# extract the data and give it useful col names
vwchourlyT2 <- CR1000_EIsoilT2[vwcrows, vwccols]
names(vwchourlyT2) <- vwcnamesT2

## bind T1 and T2 in wide format, make useful columns

vwchourly <- full_join(vwchourlyT1, vwchourlyT2, by="TIMESTAMP")
# vwchourly <- cbind(vwchourlyT1, vwchourlyT2[,2:17])

# replace 7999 values with NA
vwchourly[vwchourly==7999] <- NA
  ## because the datalogger uses 7999 for NA data

# make separate columns for date and time
vwchourly$TIMESTAMP2 <- parse_date_time(vwchourly$TIMESTAMP, orders="mdy HM") # as date
# get hour
vwchourly$Hour <- gsub( ".* ", "", vwchourly$TIMESTAMP) # as character
vwchourly$Hour2 <- hour(vwchourly$TIMESTAMP2) # as int
# get date
vwchourly$Date <- gsub( " .*$", "", vwchourly$TIMESTAMP) # as character
vwchourly$Date2 <- mdy(vwchourly$Date) # as date

## convert to long format

vwchourlylong <- gather(vwchourly, SensorID, VWC, Sensor01:Sensor35)
vwchourlylong$SensorID <- as.character(vwchourlylong$SensorID)
vwchourlylong$VWC <- as.numeric(vwchourlylong$VWC)

## get daily mean, sd, max, min
vwcdailylong <- ddply(vwchourlylong,.(Date2, SensorID),
             summarize,
             avgVWC=mean(VWC, strna.rm = TRUE),
             sdVWC=sd(VWC, na.rm = TRUE),
             seVWC=sqrt(var(VWC,na.rm=TRUE)/length(na.omit(VWC))),
             maxVWC=max(VWC, na.rm = TRUE),
             minVWC=min(VWC, na.rm = TRUE),
             .progress="text") # na.rm=T already in the function definition for ste()



########################################################################
# TEMP: HOURLY --> DAILY

## get T1 data

# where is the temp data?
tempcols <- c(1,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)
dataend <- dim(CR1000_EIsoilT1)[1]
temprows <- c(4:dataend)

# what are the variable names?
tempnamesT1 <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                "Sensor17","Sensor18","Sensor19")
#vwcnamesT1 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
temphourlyT1 <- CR1000_EIsoilT1[temprows, tempcols]
names(temphourlyT1) <- tempnamesT1

## get T2 data

# where is the temp data?
tempcols <- c(1,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34)
#vwccols  <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33)
dataend <- dim(CR1000_EIsoilT2)[1]
temprows <- c(4:dataend)

# what are the variable names?
tempnamesT2 <- c("TIMESTAMP","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
#vwcnamesT2 <- as.character(CR1000_EIsoilT1[1,vwccols])

# extract the data and give it useful col names
temphourlyT2 <- CR1000_EIsoilT2[temprows, tempcols]
names(temphourlyT2) <- tempnamesT2


## bind T1, T2 and Depth in wide format, make useful columns

temphourly <- full_join(temphourlyT1, temphourlyT2, by="TIMESTAMP")
# vwchourly <- cbind(vwchourlyT1, vwchourlyT2[,2:17])

# replace 7999 values with NA
temphourly[temphourly==7999] <- NA

# make separate columns for date and time
temphourly$TIMESTAMP2 <- parse_date_time(temphourly$TIMESTAMP, orders="mdy HM") # as date
# get hour
temphourly$Hour <- gsub( ".* ", "", temphourly$TIMESTAMP) # as character
temphourly$Hour2 <- hour(temphourly$TIMESTAMP2) # as int
# get date
temphourly$Date <- gsub( " .*$", "", temphourly$TIMESTAMP) # as character
temphourly$Date2 <- mdy(temphourly$Date) # as date

## convert to long format

temphourlylong <- gather(temphourly, SensorID, Temp, Sensor01:Sensor35)
temphourlylong$SensorID <- as.character(temphourlylong$SensorID)
temphourlylong$Temp <- as.numeric(temphourlylong$Temp)

## get daily mean, sd, max, min
tempdailylong <- ddply(temphourlylong,.(Date2, SensorID),
                      summarize,
                      avgTemp=mean(Temp, na.rm = TRUE),
                      sdTemp=sd(Temp, na.rm = TRUE),
                      seTemp=ste(Temp),
                      smaxTemp=max(Temp, na.rm = TRUE),
                      minTemp=min(Temp, na.rm = TRUE),
                      .progress="text") # na.rm=T already in the function definition for ste()



########################################################################
# O2: HOURLY --> DAILY

# get O2 datalogger data
# AI-AX = sensor 1-16 (col 35-50), FG-FV = sensor 17-32 (col 163-178), JE-JG = sensor 33-35 (col 265-267)
# note that this data is all in mV until we convert it later

# where is the O2 data?
O2cols <- c(1,35:50,163:178,269:271)
dataend <- dim(CR1000_oxygenEV)[1]
O2rows <- c(4:dataend)

# what are the variable names?
O2names <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04","Sensor05","Sensor06","Sensor07","Sensor08",
                 "Sensor09","Sensor10","Sensor11","Sensor12","Sensor13","Sensor14","Sensor15","Sensor16",
                 "Sensor17","Sensor18","Sensor19","Sensor20","Sensor21","Sensor22","Sensor23","Sensor24",
                 "Sensor25","Sensor26","Sensor27","Sensor28","Sensor29","Sensor30",
                 "Sensor31","Sensor32","Sensor33","Sensor34","Sensor35")
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

O2hourlylong <- gather(O2hourly, SensorID, O2mV, Sensor01:Sensor35)
O2hourlylong$SensorID <- as.character(O2hourlylong$SensorID)
O2hourlylong$O2mV <- as.numeric(O2hourlylong$O2mV)

## convert from mV to percent using the calibration curves for each sensor

# bring in calibration curve info
O2cal <- read.csv(paste(calibrationdatapath,"O2sensorscalibration.csv",sep=""), stringsAsFactors=FALSE) 
# sensors 36-50 use average of all calibration data for now
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
                       minO2pct=min(O2pct, na.rm = TRUE),
                       .progress="text") # na.rm=T already in the function definition for ste()



##### Q for Ryan... why are the daily averages off between the hourly tab and the daily summary tab?

#################################################################
# Redox processing (based on VWC processing)

## get data from "r"
redoxRcols <- c(1,3,7,11,15,19,23,27,31)
dataend <- dim(CR1000_redoxR)[1]
redoxrows <- c(4:dataend)
redoxRnames <- c("TIMESTAMP","Sensor01","Sensor02","Sensor03","Sensor04",
                 "Sensor05","Sensor06","Sensor07","Sensor08")

# extract the data and give it useful col names
redoxR15mins <- CR1000_redoxR[redoxrows, redoxRcols]
names(redoxR15mins) <- redoxRnames

## get data from "v"
redoxVcols <- c(1,3,7,11,15)
dataend <- dim(CR1000_redoxV)[1]
redoxrows <- c(4:dataend)
redoxVnames <- c("TIMESTAMP","Sensor09","Sensor10","Sensor11","Sensor12")

# extract the data and give it useful col names
redoxV15mins <- CR1000_redoxV[redoxrows, redoxVcols]
names(redoxV15mins) <- redoxVnames

## bind T1 and T2 in wide format, make useful columns

redox15mins <- full_join(redoxR15mins, redoxV15mins, by="TIMESTAMP")
redox15mins_unique <- unique(redox15mins) # removes perfect duplicate rows-not sure why these exist?

# replace 7999 values with NA
redox15mins_unique[redox15mins_unique==7999] <- NA
## because the datalogger uses 7999 for NA data - not sure if this is true/needed for redox files?

# make separate columns for date and time
redox15mins_unique$TIMESTAMP2 <- parse_date_time(redox15mins_unique$TIMESTAMP, orders="mdy HM") # as date
# get hour
redox15mins_unique$Hour <- gsub( ".* ", "", redox15mins_unique$TIMESTAMP) # as character
redox15mins_unique$Hour2 <- hour(redox15mins_unique$TIMESTAMP2) # as int
# get date
redox15mins_unique$Date <- gsub( " .*$", "", redox15mins_unique$TIMESTAMP) # as character
redox15mins_unique$Date2 <- mdy(redox15mins_unique$Date) # as date

## convert to long format

redox15minslong <- gather(redox15mins_unique, SensorID, SEVolt, 
                          Sensor01:Sensor12)
redox15minslong$SensorID <- as.character(redox15minslong$SensorID)
redox15minslong$SEVolt <- as.numeric(redox15minslong$SEVolt)

# add column for topolocation
definetopo <- data.frame(SensorID = c("Sensor01","Sensor02","Sensor03","Sensor04",
                                      "Sensor05","Sensor06","Sensor07","Sensor08",
                                      "Sensor09","Sensor10","Sensor11","Sensor12"),
                         TopoLocation = as.factor(c("R","R","R","R",
                                                "S","S","S","S",
                                                "V","V","V","V"))
                          )
redox15minslong <- full_join(redox15minslong, definetopo)

# get hourly summaries
redox_hourdate <- redox15minslong
redox_hourdate$TIMESTAMP_H <- ymd_h(paste(redox_hourdate$Date2, redox_hourdate$Hour2, sep = " "))
redoxhourlylong <- ddply(redox_hourdate,.(Date2,Hour2, SensorID, TopoLocation),
                        summarize,
                        TIMESTAMP=mean(TIMESTAMP_H),
                        avgSEVolt=mean(SEVolt, strna.rm = TRUE),
                        sdSEVolt=sd(SEVolt, na.rm = TRUE),
                        seSEVolt=sqrt(var(SEVolt,na.rm=TRUE)/length(na.omit(SEVolt))),
                        maxSEVolt=max(SEVolt, na.rm = TRUE),
                        minSEVolt=min(SEVolt, na.rm = TRUE),
                        .progress="text") # na.rm=T already in the function definition for ste()



## get daily mean, sd, max, min
redoxdailylong <- ddply(redox15minslong,.(Date2, SensorID, TopoLocation),
                      summarize,
                      avgSEVolt=mean(SEVolt, strna.rm = TRUE),
                      sdSEVolt=sd(SEVolt, na.rm = TRUE),
                      seSEVolt=sqrt(var(SEVolt,na.rm=TRUE)/length(na.omit(SEVolt))),
                      maxSEVolt=max(SEVolt, na.rm = TRUE),
                      minSEVolt=min(SEVolt, na.rm = TRUE),
                      .progress="text") # na.rm=T already in the function definition for ste()



##### Did not put redox into fulldaily-not sure if this should go in fulldaily and if so how to associate sensors




########################################################################
# VWC + TEMP + O2 --> ONE DF

fulldaily1 <- full_join(tempdailylong, vwcdailylong)
fulldaily2 <- full_join(fulldaily1, O2dailylong)

# put transect info in as a column near the front for easy reading
definetransects <- read.csv(paste(calibrationdatapath,"definetransects.csv",sep=""), stringsAsFactors=FALSE)
fulldaily3 <- full_join(fulldaily2, definetransects)
# dim(fulldaily3)[2] is where TransectID is; move to early column
endcol <- dim(fulldaily3)[2]
secondlast <- endcol-1
fulldaily <- fulldaily3[,c(1:2,endcol,3:secondlast)]

# add TopoLocation
defineTopoLocation <- read.csv(paste(calibrationdatapath,"defineTopoLocation.csv",sep=""), stringsAsFactors=FALSE)
fulldaily4 <- full_join(fulldaily, defineTopoLocation)
# dim(fulldaily4)[2] is where TopoLocation is; move to early column
endcol <- dim(fulldaily4)[2]
secondlast <- endcol-1
fulldaily <- fulldaily4[,c(1:2,endcol,3:secondlast)]
fulldaily$TopoLocation <- as.factor(fulldaily$TopoLocation)


########################################################################
# DEFINE WHAT GETS ADDED TO THE RUNNING SPREADSHEET

# hourly wide for each
head(vwchourly)
head(temphourly)
head(O2hourly) # this is mV, not %
head(redox15mins_unique)

# daily wide for each
# go from long to wide using spread() from tidyr
# only do this for the average daily values, not the sd, se, min or max

vwcdailylongavg <- vwcdailylong[,c(1:3)]
vwcdailywideavg <- spread(vwcdailylongavg, SensorID, avgVWC)

tempdailylongavg <- tempdailylong[,c(1:3)]
tempdailywideavg <- spread(tempdailylongavg, SensorID, avgTemp)

O2dailylongavg <- O2dailylong[,c(1:3)]
O2dailywideavg <- spread(O2dailylongavg, SensorID, avgO2pct)

redoxdailylongavg <- redoxdailylong[,c(1:2,4)]
redoxdailywideavg <- spread(redoxdailylongavg,SensorID, avgSEVolt)

redoxhourlylongavg <- redoxhourlylong[,c(3,5,6)]
redoxhourlywideavg <- spread(redoxhourlylongavg,SensorID,avgSEVolt)

head(tempdailywideavg)
head(vwcdailywideavg)
head(O2dailywideavg)
head(redoxdailywideavg)

# daily long combined dataset
head(fulldaily)


########################################################################
# SAVE AS CSVS

# hourly wide for each

# make ok to write to CSV
vwchourly$TIMESTAMP2 <- as.character(vwchourly$TIMESTAMP2)
vwchourly$Date2 <- as.character(vwchourly$Date2)
temphourly$TIMESTAMP2 <- as.character(temphourly$TIMESTAMP2)
temphourly$Date2 <- as.character(temphourly$Date2)
O2hourlylong$TIMESTAMP2 <- as.character(O2hourlylong$TIMESTAMP2)
O2hourlylong$Date2 <- as.character(O2hourlylong$Date2)
redox15minslongTIMESTAMP2 <- as.character(redox15mins_unique$TIMESTAMP2)
redox15minslong$Date2 <- as.character(redox15mins_unique$Date2)
redoxhourlylong$TIMESTAMP <- as.character(redoxhourlywideavg$TIMESTAMP)

# write to csv
write.csv(vwchourly, file=paste(outputdatapath, "vwchourly.csv", sep = ""), row.names=FALSE)
write.csv(temphourly, file=paste(outputdatapath, "temphourly.csv", sep = ""), row.names=FALSE)
write.csv(O2hourlylong, file=paste(outputdatapath, "O2hourly.csv", sep = ""), row.names=FALSE)
write.csv(redox15minslong, file=paste(outputdatapath, "redox15mins.csv", sep= ""), row.names=FALSE)
write.csv(redoxhourlylong, file=paste(outputdatapath, "redoxhourly.csv", sep=""), row.names=FALSE)

# daily wide for each

# make ok to write to CSV
vwcdailywideavg$Date2 <- as.character(vwcdailywideavg$Date2)
tempdailywideavg$Date2 <- as.character(tempdailywideavg$Date2)
O2dailywideavg$Date2 <- as.character(O2dailywideavg$Date2)
redoxdailywideavg$Date2 <- as.character(redoxdailywideavg$Date2)

# write to csv
write.csv(vwcdailywideavg, file=paste(outputdatapath, "vwcdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(tempdailywideavg, file=paste(outputdatapath, "tempdailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(O2dailywideavg, file=paste(outputdatapath, "O2dailywideavg.csv", sep = ""), row.names=FALSE)
write.csv(redoxdailywideavg, file=paste(outputdatapath, "redoxdailywideavg.csv", sep =""), row.names=FALSE)
# daily long combined dataset

# make ok to write to CSV
fulldaily$Date2 <- as.character(fulldaily$Date2)

# write to csv
write.csv(fulldaily, file=paste(outputdatapath, "fulldaily.csv", sep = ""), row.names=FALSE)


##### FOR NOW, CHRISTINE IS JUST PASTING THESE INTO THE APPROPRIATE PLACE ON THE RUNNING EXCEL SHEET



# if you want to test the data in this sheet, graph via the below

# ########################################################################
# # SUMMARY STATS: O2, MOISTURE ACROSS TRANSECTS AT EACH DATE
# 
# # summarySE using plyr
# source("C:/Users/jstar_000/Desktop/Data analysis/summarySE.r")
# "DayCount", "Drought"
# # summarySE O2
#summarytab1tmp <- summarySE(data=fulldaily, measurevar="O2", groupvars=c("Date2", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE) # this function is just producing warnings
# # summarySE moisture
#summarytab2tmp <- summarySE(data=fulldaily, measurevar="avgVWC", c("Date2", "TopoLocation"), na.rm=TRUE, renameallcols=TRUE)
# 
# 
# ########################################################################
# # EXPLORATORY FIGURES: TIME SERIES
# 
#topocolors <- c("navy","blue","dark green","green","yellow","orange","red")
#topobreaks <- c("1","2","3","4","5","6","7")
#topolabs <- c("Ridge","2","3","4","5","6","Valley")
# 
# # O2 by date (mean and se)
# p1 <- ggplot(summarytab1, aes(x=Date, y=meanO2, color=TopoLocation)) + 
#geom_point() + geom_errorbar(aes(ymin=meanO2-seO2, ymax=meanO2+seO2), alpha=0.5) +
#ylab("Soil O2 (Mean Fraction +/- Standard Error)") + theme_bw() +
#theme(axis.text.x=element_text(angle=90)) + 
#scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y"))  + geom_line()
# 
# # moisture by date (mean and se)
#p2 <- ggplot(summarytab2, aes(x=Date2, y=meanavgVWC, color=TopoLocation)) + geom_point() + 
#geom_errorbar(aes(ymin=meanavgVWC-seavgVWC, ymax=meanavgVWC+seavgVWC), alpha=0.5) + 
#ylab("Soil Moisture (Mean Fraction +/- Standard Error)") + theme_bw() + 
#theme(axis.text.x=element_text(angle=90)) + scale_x_datetime(breaks = date_breaks("4 weeks"), labels = date_format("%d-%m-%y")) 
#+ geom_line()


ggplot(fulldaily,aes(x=as.Date(Date2),y=avgO2pct,color=TopoLocation)) + 
  geom_point() +
  scale_x_date(limits=as.Date(c("2017-01-01","2017-09-01"))) +
  labs(x="Date",y="O2 concentration")
ggsave("SurfaceO2.jpg",path=outputdatapath)

ggplot(fulldaily,aes(x=as.Date(Date2),y=avgTemp,color=TopoLocation)) + 
  geom_point() +
  labs(x="Date",y="Soil Temp")
ggsave("SurfaceTemp.jpg",path=outputdatapath)

ggplot(fulldaily,aes(x=as.Date(Date2),y=avgVWC,color=TopoLocation)) + 
  geom_point() +
  labs(x="Date",y="Volumetric Water Content")
ggsave("SurfaceVWC.jpg",path=outputdatapath)

ggplot(redoxdailylong,aes(x=as.Date(Date2),y=avgSEVolt,color=TopoLocation))+
  geom_point() +
  labs(x="Date",y="SEVolt")
ggsave("Redox.jpg",path=outputdatapath)
