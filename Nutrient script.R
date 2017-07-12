#### Calculates soil Fe, P, moisture and pH
## Jordan Stark, May 2017
## Main input file should be a .csv of data for each sample with colums titled:
 # Sample
 # Site
 # AvgDepth
 # Tin wt
 # Soil wt moisture -- just the soil, not the tin
 # TotalDry wt      -- soil+tin
 # pH
 # Soil wt P
 # NaHCO3 wt
 # NaOH wt
 # Pt NaHCO3
 # Pi NaHCO3
 # Pt NaOH
 # Pi NaOH
 # HCl wt
 # Soil wt Fe
 # Water
 # Water_dilut   -- dilution factor (1 if no dilution)
 # Hydrox
 # Hydrox_dilut

######## need to add N stuff
 
## blanks should be in file as samples with no data or NA for all inapplicable columns
  # write "Blank" in the Sample column
## if no blanks, add one row labeled "Blank" with zero absorbance

## Also need input files for each standard curve: 
 # P std curve : mg , Pi , Pt
 # Fe std curve : mmol , H2O , Hydrox

## For all input files, other columns are ok
## Outputs will be a .csv with soil results, also makes basic graphs





# Where is the data? 

datapath <- "~/Desktop/Misc data/Soil script test/"

# standards for Fe
Festd    <- read.csv(paste(datapath,"Fe std 3.csv",sep=""),
                        stringsAsFactors = FALSE)


# standards for P
Pstd    <- read.csv(paste(datapath,"P std curve.csv",sep=""),
                     stringsAsFactors = FALSE)


# main dataset
rawdata <- read.csv(paste(datapath,"OldFeData_std3.csv",sep=""),
                    stringsAsFactors=FALSE)

# this is where the results will go
fullresults <- data.frame(Sample = rawdata$Sample,
                          Site = rawdata$Site,
                          AvgDepth = rawdata$AvgDepth)
# add here any other columns for identifying data (eg rep)

# this function is used to set nutrient values <0 to 0 

CheckZero <- function(values) {
  for (i in 1:length(values)){
    if(is.na(values[i])) {values[i] <- NA} else
    if(values[i]<0) {
                      warning(paste(values[i],"set to zero"))
                      values[i] <- 0 
                      }
  }
  return(values)
} 

############### Process soil moisture and pH data ###############

TotalWet.wt   <- rawdata$Tin.wt + rawdata$Soil.wt.moisture
MoistureFract <- (TotalWet.wt - rawdata$TotalDry.wt)/rawdata$Soil.wt.moisture

fullresults$MoistureFract <- MoistureFract
fullresults$pH            <- rawdata$pH

#################################################################

####################### Process Fe data #########################

# Calculate total Fe mmol values from Hydrox extraction
HydroxCurve  <- lm(Festd$Hydrox~Festd$mmol)
Hydrox.m     <- summary(HydroxCurve)$coefficients[2]
Hydrox.b     <- summary(HydroxCurve)$coefficients[1]
Hydrox.Blank <- mean(rawdata$Hydrox[rawdata$Sample=="Blank"])

Hydrox.Blank.mmol <- (Hydrox.Blank-Hydrox.b)/Hydrox.m
if(Hydrox.Blank.mmol < 0 || is.na(Hydrox.Blank.mmol)) {Hydrox.Blank.mmol <- 0}

TotalFe.mmol.raw <- ((rawdata$Hydrox-Hydrox.b)/Hydrox.m) - Hydrox.Blank.mmol
TotalFe.mmol.cor <- TotalFe.mmol.raw*rawdata$Hydrox_dilut # corrects for dilutions

# Calculate Total Fe mg/gram dry soil
TotalFe.mg     <- TotalFe.mmol.cor*(rawdata$HCl.wt/1000)*55.84
TotalFe.mg.gds <- TotalFe.mg/(rawdata$Soil.wt.Fe*(1-MoistureFract))
TotalFe.mg.gds <- CheckZero(TotalFe.mg.gds)

# Calculate Fe(II) mmol values from water extraction
WaterCurve  <- lm(Festd$H2O~Festd$mmol)
Water.m     <- summary(WaterCurve)$coefficients[2]
Water.b     <- summary(WaterCurve)$coefficients[1]
Water.Blank <- mean(rawdata$Water[rawdata$Sample=="Blank"])

Water.Blank.mmol <- (Water.Blank-Water.b)/Water.m
if(Water.Blank.mmol < 0 || is.na(Water.Blank.mmol)) {Water.Blank.mmol <- 0}

FeII.mmol.raw <- (rawdata$Water-Water.b)/Water.m - Water.Blank.mmol
FeII.mmol.cor <- FeII.mmol.raw*rawdata$Water_dilut

# Calculate Fe(II) mg/gram dry soil
FeII.mg     <- FeII.mmol.cor*(rawdata$HCl.wt/1000)*55.84
FeII.mg.gds <- FeII.mg/(rawdata$Soil.wt.Fe*(1-MoistureFract))
FeII.mg.gds <- CheckZero(FeII.mg.gds)

# Calculate Fe(III) mg/gds
FeIII.mg.gds <- TotalFe.mg.gds-FeII.mg.gds
FeIII.mg.gds <- CheckZero(FeIII.mg.gds)

# Bind water and hydrox results to fullresults
fullresults$TotalFe.mg.gds <- TotalFe.mg.gds
fullresults$FeII.mg.gds    <- FeII.mg.gds
fullresults$FeIII.mg.gds   <- FeIII.mg.gds


#############################################################

################# Process P(i) and P(t) data ################

# Calculate Pi mg values
PiCurve <- lm(Pstd$Pi~Pstd$mg)
Pi.m    <- summary(PiCurve)$coefficients[2]
Pi.b    <- summary(PiCurve)$coefficients[1]

Pi.NaHCO3.mg.L <- (rawdata$Pi.NaHCO3-Pi.b)/Pi.m
Pi.NaOH.mg.L   <- (rawdata$Pi.NaOH-Pi.b)/Pi.m

Pi.NaHCO3.mg   <- Pi.NaHCO3.mg.L*(rawdata$NaHCO3.wt/1000)
Pi.NaOH.mg     <- Pi.NaOH.mg.L*(rawdata$NaOH.wt/1000)

# Calculate Pt mg values
PtCurve <- lm(Pstd$Pt~Pstd$mg)
Pt.m    <- summary(PtCurve)$coefficients[2]
Pt.b    <- summary(PtCurve)$coefficients[1]

Pt.NaHCO3.mg.L <- (rawdata$Pt.NaHCO3-Pt.b)/Pt.m
Pt.NaOH.mg.L   <- (rawdata$Pt.NaOH-Pt.b)/Pt.m

Pt.NaHCO3.mg   <- Pt.NaHCO3.mg.L*(rawdata$NaHCO3.wt/1000)
Pt.NaOH.mg     <- Pt.NaOH.mg.L*(rawdata$NaOH.wt/1000)

# Calculate mg/g dry soil
Pi.NaHCO3.mg.gds <- Pi.NaHCO3.mg/(rawdata$Soil.wt.P*(1-MoistureFract)) 
Pi.NaHCO3.mg.gds <- CheckZero(Pi.NaHCO3.mg.gds)

Pi.NaOH.mg.gds   <- Pi.NaOH.mg/(rawdata$Soil.wt.P*(1-MoistureFract)) 
Pi.NaOH.mg.gds   <- CheckZero(Pi.NaOH.mg.gds)

Pt.NaHCO3.mg.gds <- Pt.NaHCO3.mg/(rawdata$Soil.wt.P*(1-MoistureFract)) 
Pt.NaHCO3.mg.gds <- CheckZero(Pt.NaHCO3.mg.gds)

Pt.NaOH.mg.gds   <- Pt.NaOH.mg/(rawdata$Soil.wt.P*(1-MoistureFract)) 
Pt.NaOH.mg.gds   <- CheckZero(Pt.NaOH.mg.gds)


# Calculate mg/gds for organic P
Po.NaHCO3.mg.gds <- Pt.NaHCO3.mg.gds - Pi.NaHCO3.mg.gds
Po.NaHCO3.mg.gds <- CheckZero(Po.NaHCO3.mg.gds)

Po.NaOH.mg.gds   <- Pt.NaOH.mg.gds - Pi.NaOH.mg.gds
Po.NaOH.mg.gds   <- CheckZero(Po.NaOH.mg.gds)

# Sum NaHCO3 and NaOH for all sections
Pi.Sum.mg.gds <- Pi.NaHCO3.mg.gds + Pi.NaOH.mg.gds
Pt.Sum.mg.gds <- Pt.NaHCO3.mg.gds + Pt.NaOH.mg.gds
Po.Sum.mg.gds <- Po.NaHCO3.mg.gds + Po.NaOH.mg.gds

# Bind P data to FullResults
fullresults$Pi.NaHCO3.mg.gds <- Pi.NaHCO3.mg.gds
fullresults$Pi.NaOH.mg.gds   <- Pi.NaOH.mg.gds
fullresults$Pi.Sum.mg.gds    <- Pi.Sum.mg.gds

fullresults$Pt.NaHCO3.mg.gds <- Pt.NaHCO3.mg.gds
fullresults$Pt.NaOH.mg.gds   <- Pt.NaOH.mg.gds
fullresults$Pt.Sum.mg.gds    <- Pt.Sum.mg.gds

fullresults$Po.NaHCO3.mg.gds <- Po.NaHCO3.mg.gds
fullresults$Po.NaOH.mg.gds   <- Po.NaOH.mg.gds
fullresults$Po.Sum.mg.gds    <- Po.Sum.mg.gds

# add section for total phosphorus (NaOH + bicarb)?

##########################################################


############### Process KCl and N-min data ###############

# not sure how to process this

##########################################################

############### Write data to file ###############

write.csv(fullresults,
          paste(datapath,"Soil nutrient results std 3.csv",sep=""),
          row.names = FALSE)

##########################################################


##### Figures #####
# currently not set up to deal with replicates

library(ggplot2)

DepthPlot <- function(xvals, label){
  ggplot(fullresults,aes(x=xvals,y=fullresults$AvgDepth,color=fullresults$Site))+
    geom_path() +
    scale_y_reverse() +
    theme_classic() +
    ylab("Depth (cm below ground)") +
    xlab(label) +
    guides(color=guide_legend(title=NULL))
}

# Basic data
DepthPlot(fullresults$MoistureFract,"Soil moisture")
DepthPlot(fullresults$pH, "Soil pH")

# Fe
DepthPlot(fullresults$TotalFe.mg.gds, "Total Fe mg/gds")
DepthPlot(fullresults$FeII.mg.gds, "Fe(II) mg/gds")
DepthPlot(fullresults$FeIII.mg.gds, "Fe(III) mg/gds")

# P
DepthPlot(fullresults$Pi.NaHCO3.mg.gds, "Inorganic P NaHCO3 mg/gds")
DepthPlot(fullresults$Pi.NaOH.mg.gds, "Inorganic P NaOH mg/gds")
DepthPlot(fullresults$Pi.Sum.mg.gds, "Inorganic P NaHCO3+NaOH mg/gds")

DepthPlot(fullresults$Pt.NaHCO3.mg.gds, "Total P NaHCO3 mg/gds")
DepthPlot(fullresults$Pt.NaOH.mg.gds, "Total P NaOH mg/gds")
DepthPlot(fullresults$Pt.Sum.mg.gds, "Total P NaHCO3+NaOH mg/gds")

DepthPlot(fullresults$Po.NaHCO3.mg.gds, "Organic P NaHCO3 mg/gds")
DepthPlot(fullresults$Po.NaOH.mg.gds, "Organic P NaOH mg/gds")
DepthPlot(fullresults$Po.Sum.mg.gds, "Organic P NaHCO3+NaOH mg/gds")
