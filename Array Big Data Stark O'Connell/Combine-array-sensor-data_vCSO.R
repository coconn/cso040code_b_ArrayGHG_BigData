# Combine-array-sensor-data
# based on "Process-arraysensorsdf-Sensor-Data-RCode" 
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
DataArchivepath <- "~/Desktop/Datalogger_downloads/DataArchive/" 
# old data; this should never change unless changing computers

# christine version; uncomment when CSO doing things
#DataArchivepath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceDataArchive/"

NewDatapath      <- "~/Desktop/Datalogger_downloads/4-12-17/Surface results/" 
# this should change each time data is dowloaded

# christine version; uncomment when CSO doing things
#NewDatapath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceNewest/"
# when christine was adding in the very old data
#NewDatapath <- "~/Documents/GITHUB/cso044code_HotSpotsHotMoments/HotSpotsHotMomentsAnalysis/HotSpotsHotMoments-Data-Raw/Sensors/SurfaceOutOfDate/OldestCSOHas/renamedtomatchcode/"

# create master list of file names
allfiles <- c("vwchourly","temphourly","O2hourly","fulldaily",
              "O2dailywideavg", "tempdailywideavg","vwcdailywideavg")


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
  CompleteFiles[[i]] <- join(OldFiles[[i]],NewFiles[[i]],
                             type="full")
  names(CompleteFiles)[i] <- paste("Complete",allfiles[i],sep="")
  OldFiles[i] <- "done"
}

# Save all data
for(i in 1:7){
  write.csv(CompleteFiles[[i]],
           file=paste(DataArchivepath,allfiles[i],".csv",sep=""),
           row.names=FALSE)
}



# Figures for sanity check

library(ggplot2)
library(lubridate)

fulldaily <- CompleteFiles[[4]]


ggplot(fulldaily,aes(x=as.Date(Date2),y=avgO2pct,color=TopoLocation)) + 
  geom_point() +
  labs(x="Date",y="O2 concentration")
ggsave("O2.pdf", path = DataArchivepath)


ggplot(fulldaily,aes(x=as.Date(Date2),y=avgTemp,color=TopoLocation)) + 
  geom_point() +
  labs(x="Date",y="Soil Temp")
ggsave("Soil Temp.pdf", path = DataArchivepath)

ggplot(fulldaily,aes(x=as.Date(Date2),y=avgVWC,color=TopoLocation)) + 
  geom_point() +
  labs(x="Date",y="Volumetric Water Content")
ggsave("VWC.pdf", path = DataArchivepath)
