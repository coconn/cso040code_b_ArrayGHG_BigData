#### Transform units on gas flux data ####
# Jordan Stark, July 2017

# currently, this function works to transform:
  # quantities: g, kg, Gt, mol, umol, gC (for CO2 only)
  # areas: m2, ha
  # time: sec, min, hour, day, year
  # gases: CO2, CH4, N2O
    # if no mol conversion, gas ID not needed

##########################################

FluxUnits <- function(fluxrate, quant_in, area_in, time_in, quant_out, area_out, time_out, gasID=NA){
  transform <- 1
  
  accepted_quant <- c("g", "kg", "Gt", "mol", "umol", "gC")
  accepted_area  <- c("m2", "ha")
  accepted_time  <- c("sec", "min", "hour", "day", "year")
  accepted_gas   <- c("CO2","CH4","N2O")
  
  gas_weights    <- data.frame(gas=accepted_gas,
                               weight=c(44.01,16.04,44.01))
  
  if(quant_in=="mol" | quant_in=="umol" |
     quant_out=="mol" | quant_out=="umol"){ # is there a mol conversion?
        if(gasID %in% accepted_gas) { # is this gas supported?
            gpermol        <- gas_weights$weight[gas_weights$gas==gasID] # set molecular wt
        } else {stop("Molecular weight not found; do not use mol conversions")}
  }
  
  if(quant_in=="gC" | quant_out=="gC"){ #Need to convert to/from gC?
    if(gasID=="CO2"){ # is this CO2?
      gCperg   <- 12.01/44.01
      gCpermol <- 12.01
    } else {stop("gC only valid for CO2 conversions")}
  }
  

# Quantity
  
  if(quant_in != quant_out){  # quantity needs tranformation?
    if(quant_in %in% accepted_quant & quant_out %in% accepted_quant){ # valid units?
      # Transform quantities
      if(quant_in=="g") {
        if(quant_out=="kg")   {transform <- transform/1000}
        if(quant_out=="Gt")   {transform <- transform/10^15}
        if(quant_out=="mol")  {transform <- transform/gpermol}
        if(quant_out=="umol") {transform <- transform*10^6/gpermol}
        if(quant_out=="gC")   {transform <- transform*gCperg}
      }
      if(quant_in=="kg"){
        if(quant_out=="g")    {transform <- transform*1000}
        if(quant_out=="Gt")   {transform <- transform/10^12}
        if(quant_out=="mol")  {transform <- transform*1000/gpermol}
        if(quant_out=="umol") {transform <- transform*10^9/gpermol}
        if(quant_out=="gC")   {transform <- transform*1000*gCperg}
      }
      if(quant_in=="Gt"){
        if(quant_out=="g")    {transform <- transform*10^15}
        if(quant_out=="kg")   {transform <- transform*10^12}
        if(quant_out=="mol")  {transform <- transform*10^15/gpermol}
        if(quant_out=="umol") {transform <- transform*10^21/gpermol}
        if(quant_out=="gC")   {transform <- transform*10^15*gCperg}
      }
      if(quant_in=="mol"){ 
        if(quant_out=="g")    {transform <- transform*gpermol}
        if(quant_out=="kg")   {transform <- transform*gpermol/1000}
        if(quant_out=="Gt")   {transform <- transform*gpermol/10^15}
        if(quant_out=="umol") {transform <- transform*10^6}
        if(quant_out=="gC")   {transform <- transform*gCpermol}
      }
      if(quant_in=="umol"){ 
        if(quant_out=="g")    {transform <- transform*gpermol/10^6}
        if(quant_out=="kg")   {transform <- transform*gpermol/10^9}
        if(quant_out=="Gt")   {transform <- transform*gpermol/10^21}
        if(quant_out=="mol")  {transform <- transform/10^6}
        if(quant_out=="gC")   {transform <- transform*gCpermol/10^6}
      }
      if(quant_in=="gC"){
        if(quant_out=="g")    {transform <- transform/gCperg}
        if(quant_out=="kg")   {transform <- transform/(gCperg*1000)}
        if(quant_out=="Gt")   {transform <- transform/(gCperg*10^15)}
        if(quant_out=="mol")  {transform <- transform/gCpermol}
        if(quant_out=="umol") {transform <- transform*10^6/gCpermol}
      }
    } else {stop("Invalid quantity units")}
  }
  
# Area

  if(area_in != area_out){ # area needs transformation?
    if(area_in %in% accepted_area & area_out %in% accepted_area){ # valid units?
      # Transform area
      if(area_in=="m2"){
        if(area_out=="ha")   {transform <- transform*10000}
      }
      if(area_in=="ha"){
        if(area_out=="m2")   {transform <- transform/10000}
      }
    } else {stop("Invalid area units")}
  }

# Time
  
  if(time_in != time_out){ # time needs transformation?
    if(time_in %in% accepted_time & time_out %in% accepted_time){ # valid units?
      # Transform time
      if(time_in=="sec"){
        if(time_out=="min")   {transform <- transform*60}
        if(time_out=="hour")  {transform <- transform*60*60}
        if(time_out=="day")   {transform <- transform*60*60*24}
        if(time_out=="year")  {transform <- transform*60*60*24*365} #no leap years
      }
      if(time_in=="min"){
        if(time_out=="sec")   {transform <- transform/60}
        if(time_out=="hour")  {transform <- transform*60}
        if(time_out=="day")   {transform <- transform*60*24}
        if(time_out=="year")  {transform <- transform*60*24*365}
      }
      if(time_in=="hour"){
        if(time_out=="sec")   {transform <- transform/(60*60)}
        if(time_out=="min")   {transform <- transform/60}
        if(time_out=="day")   {transform <- transform*24}
        if(time_out=="year")  {transform <- transform*24*365}
      }
      if(time_in=="day"){
        if(time_out=="sec")   {transform <- transform/(60*60*24)}
        if(time_out=="min")   {transform <- transform/(60*24)}
        if(time_out=="hour")  {transform <- transform/24}
        if(time_out=="year")  {transform <- transform*365}
      }
      if(time_in=="year"){
        if(time_out=="sec")   {transform <- transform/(60*60*24*365)}
        if(time_out=="min")   {transform <- transform/(60*24*365)}
        if(time_out=="hour")  {transform <- transform/(24*365)}
        if(time_out=="day")   {transform <- transform/365}
      }
    } else {stop("Invalid time units")}
  }
  
  # apply tranformation to data
  return(fluxrate*transform)
}

## testing

FluxUnits(5,"g","m2","day","g","m2","day","N2O")




