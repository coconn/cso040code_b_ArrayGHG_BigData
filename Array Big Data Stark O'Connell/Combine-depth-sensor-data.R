# Combine-depth-sensor-data
# based on "Process-depthsensorsdf-Sensor-Data-RCode" 
#     (run that file on new data downloads first to create inputs!)
# 
# Combines data from most recent download date ("NewDatapath")
#  with old array data ("DataArchivepath")
#  and saves complete dataset in the DataArchive
#  for seven files:
#                   fulldaily.csv
#                   O2dailywideavg.csv
#                   O2hourly.csv
#                   tempdailywideavg.csv
#                   temphourly.csv
#                   vwcdailywideavg.csv
#                   vwchourly.csv

######################################################################

# load packages
library(plyr)
library(dplyr)


# locate datasets
DataArchivepath <- "~/Desktop/Datalogger_downloads/DepthDataArchive/" 
# old data; this should never change unless changing computers

# christine version; uncomment when CSO doing things
#DataArchivepath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceDataArchive/"

NewDatapath      <- "~/Desktop/Datalogger_downloads/7-3-17/Depth results/" 
# this should change each time data is dowloaded

# christine version; uncomment when CSO doing things
#NewDatapath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceNewest/"
# when christine was adding in the very old data
#NewDatapath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceOutOfDate/OldestCSOHas/renamedtomatchcode/"

# create master list of file names
allfiles <- c("fulldaily","O2dailywideavg","O2hourly",
              "tempdailywideavg","temphourly",
              "vwcdailywideavg","vwchourly")


# bring in old data
OldFiles <- list()
for(i in 1:7) {
  OldFiles[[i]] <- as.data.frame(read.csv(paste(DataArchivepath,allfiles[i],".csv",sep=""), stringsAsFactors=FALSE))
  names(OldFiles)[i] <- paste("Old",allfiles[i],sep="")
}



# bring in new data
NewFiles <- list()
for(i in 1:7) {
  NewFiles[[i]] <- as.data.frame(read.csv(paste(NewDatapath,allfiles[i],".csv",sep=""), stringsAsFactors=FALSE))
  names(NewFiles)[i] <- paste("New",allfiles[i],sep="")
}

# Bind datasets
CompleteFiles <- list()

for(i in 1:7) {
  stopifnot(names(OldFiles[[i]])==names(NewFiles[[i]]))
  CompleteFiles[[i]] <- unique(join(OldFiles[[i]],NewFiles[[i]],type="full"))
  names(CompleteFiles)[i] <- paste("Complete",allfiles[i],sep="")
}



# Figures for sanity check

library(ggplot2)
library(lubridate)

fulldaily <- CompleteFiles[[1]]
fulldaily$realDate <- parse_date_time(fulldaily$Date2,orders="ymd")

ggplot(fulldaily,aes(x=realDate,y=avgO2pct,color=as.factor(Depth))) + 
  geom_point() +
  facet_grid(TopoLocation~.) +
  labs(x="Date",y="O2 concentration") +
  scale_y_continuous(limits=c(-5,25))
ggsave("Depth O2zoomed.pdf", path = DataArchivepath)
#  scale_y_continuous(limits=c(-2,22)) +

ggplot(fulldaily,aes(x=realDate,y=avgTemp,color=as.factor(Depth))) + 
  geom_point() +
  facet_grid(TopoLocation~.) +
  labs(x="Date",y="Soil Temp")
ggsave("Depth Soil Temp.pdf", path = DataArchivepath)

ggplot(fulldaily,aes(x=realDate,y=avgVWC,color=as.factor(Depth))) + 
  geom_point() +
  facet_grid(TopoLocation~.) +
  labs(x="Date",y="Volumetric Water Content")
ggsave("Depth VWC.pdf", path = DataArchivepath)

# Save all data
for(i in 1:7){
  write.csv(CompleteFiles[[i]],
            file=paste(DataArchivepath,allfiles[i],".csv",sep=""),
            row.names=FALSE)
}

