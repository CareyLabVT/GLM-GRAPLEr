# Post-processing script for GLM simulations in the GRAPLEr
# Set up to extract multiple summary response variables from one simulation period in dataset CSV format
# Written by Cayelan Carey, last edits on 1 May 2016
# Updated by Arianna Krinos, last edits on 15 April 2017

#Note: need to edit the experiments directory
#setwd('C:/Users/arian_000/Documents/Spring 2017/Carey Lab/PRAGMA_Sunapee/')

library(glmtools)
library(GLMr) 
library(ncdf4)
#'@import rLakeAnalyzer
#'@import ncdf4

# Read the simulations for this experiment
SimDir = paste(getwd(), "Results", sep='/')
setwd(SimDir)
SimFile = paste(SimDir,'output.nc',sep = "/")
listOfFiles = character(0)
inflow = read.csv("inflow_Sunapee_2010_2014_30Mar17.csv", header = TRUE) # Change based on sim
outflow = read.csv("outflow_Sunapee_2010_2014_30Mar17.csv", header = TRUE) # Change based on sim
overflow = read.csv("overflow.csv", header = TRUE)
colnames(overflow) <- c("time", "FLOW", "oxy", "something")

myOriginalDataPCAll = get_var(SimFile,'temp')
days <- (as.POSIXct(strptime(myOriginalDataPCAll$DateTime, format = "%Y-%m-%d"), format = "%Y-%m-%d"))
uniquedays <- unique(as.POSIXct(strptime(myOriginalDataPCAll$DateTime, format = "%Y-%m-%d"), format = "%Y-%m-%d"))
dates <- as.numeric(format(myOriginalDataPCAll$DateTime, '%Y'))

months <- as.numeric(format(myOriginalDataPCAll$DateTime, '%m'))
curr <- dates[1]
datesNew <- c(rep(NA, 5)) # Replace with number of years 
counter = 2
datesNew[1] = curr
for (i in 1:length(dates)+1) {
  if (!is.na(dates[i]) && dates[i] != curr) {
    datesNew[counter] = dates[i] 
    counter = counter + 1
    curr <- dates[i]
  }
}

# The following code is partially reproduced from the USGS github repository 

#' @importFrom ncdf4 nc_open
Sim_glmnc <- nc_open (SimFile, readunlim = TRUE)
NS	<- 	ncvar_get(Sim_glmnc, "NS")
elev <- ncvar_get(Sim_glmnc, "z")
time <- myOriginalDataPCAll$DateTime
#' @importFrom ncdf4 nc_close
nc_close(Sim_glmnc)

surface_height <- vector(mode = "numeric",length = length(NS))
for (j in 1:length(NS)){
  surface_height[j] <- elev[NS[j],j]
}

glm_surface <- data.frame('DateTime'=time, 'surface_height'=surface_height)

glm_surface <- resample_sim(df = glm_surface, time, method = "match", precision = "hours")


# End section from USGS

VarsToAnalyze = c('temp','OXY_oxy','TOT_tn','TOT_tp','PHY_TPHYS','PHY_TCHLA','PHY_CYANOPCH1','PHY_CYANONPCH2',
                  'PHY_CHLOROPCH3','PHY_DIATOMPCH4','PHY_PPR','TOT_tss', 'hice', 'NIT_sed_nit', 'NIT_sed_amm', 'OXY_sed_oxy', 'PHS_sed_frp', 'NIT_amm',
                  'NIT_nit', 'PHS_frp', "NIT_denit", "NIT_nitrif") 

# Initialize vectors
myOriginalDataPC = get_var(SimFile,VarsToAnalyze[1]) # DateTime vector identical regardless of depths used 
for (i in 1:100) {
  assign(paste('temp', i, sep = ''), rep(NA, length(myOriginalDataPC$DateTime)))
}

# Create the main dataframes

# This function, when called, finds the maximum depth of the lake, thus finding the three numbers which
# should be used for the 0-2 m surface depths. 
findLakeDepth <- function (Data = FALSE){
  if (missing(Data)) {
    myOriginalDataPCAll = get_var(SimFile,VarName)
  }
  else {
    myOriginalDataPCAll = Data
  }
  allDepths = get.offsets(myOriginalDataPCAll)
  depthMax = max(round(allDepths))
  return (depthMax);
}

# Changes myOriginalDataPC to reflect the data for a single year. 
changeMyOriginalData <- function(year, myOriginalData) { 
  myOriginalData <- as.data.frame(myOriginalData)
  datesVector <- as.POSIXct(myOriginalData$DateTime, origin = "1970-01-01 00:00:00")
  datesVector <- as.numeric(format(datesVector, '%Y'))
  for (i in 1:length(datesVector)) { 
    current <- datesVector[i]
    if (!is.na(current) && current == year) {
      found <- FALSE
      for (j in i: length(datesVector)) { 
        current <- datesVector[j]
        if (current != year) {
          print('yes')
          myOriginalData <- myOriginalData[i:(j-1),]
          found <- TRUE
          break
        }
      }
      if (found == FALSE) {
        print('no')
        myOriginalData <- myOriginalData[i:j,]
      }
      break
    }
  }
  return (myOriginalData)
}

returnMaxDepthsAtTime <- function() {
  myOriginalDataPCAll = get_var(SimFile,VarName)
  allDepths = get.offsets(myOriginalDataPCAll)
  maxDepth <- findLakeDepth();
  VarName <- 'temp'
  myOriginalDataPC = get_var(SimFile,VarName, z_out = 0:(maxDepth))
  maxDepthsByTime <- matrix(NA, nrow=nrow(myOriginalDataPC), ncol=2)
  maxDepthsByTime[,1] <- as.POSIXct(myOriginalDataPC$DateTime, origin = "1970-01-01")
  for (j in 1:nrow(myOriginalDataPC)) {
    for (k in 2:ncol(myOriginalDataPC)) {
      found <- FALSE;
      if (is.na(myOriginalDataPC[j,k])) {
        thisRow <- k-1
        maxDepthsByTime[j,2] <- thisRow
        found <- TRUE;
        break;
      }
    }
    if (found == FALSE) { 
      thisRow <- maxDepth
      maxDepthsByTime[j,2] <- thisRow
    }
  }
  return (maxDepthsByTime)
}

resetVars <- function(data, posRow, dataSurf) { 
  if (!missing(dataSurf)) {
    currMax0_2 <- data[posRow]
    currDate0_2 <- myOriginalDataPCAll$DateTime[posRow]
  }
  else {
    currMax0_2 <- 0
    currDate0_2 <- myOriginalDataPCAll$DateTime[1]
  }
  currMax <- data[posRow]
  currDate <- as.POSIXct(myOriginalDataPCAll$DateTime[posRow], origin = "1970-01-01")
  return (c(currMax, currMax0_2, currDate, currDate0_2)) 
}

maxData <- resetVars(c(0), 1, c(0)) # currMax, then currMax0_2, then currDate, then currDate0_2 in vector form.
minData <- resetVars(c(100), 1, c(100))

findMax <- function(maxData, data, num, dataSurf) {
  if (!missing(dataSurf)) { 
    if (dataSurf[num] > maxData[2]) {
      currMax0_2 <- dataSurf[num]
      currDate0_2 <- myOriginalDataPCAll$DateTime[num]
    }
    else { 
      currMax0_2 <- maxData[2]
      currDate0_2 <- maxData[4]
    }
  }
  else { 
    currMax0_2 <- maxData[2]
    currDate0_2 <- maxData[4]
  }
  if (data[num] > maxData[1]) {
    currMax <- data[num]
    currDate <- myOriginalDataPCAll$DateTime[num]
  }
  else {
    currMax <- maxData[1]
    currDate <- maxData[3]
  }
  currDate <- as.POSIXct(currDate, origin = "1970-01-01")
  currDate0_2 <- as.POSIXct(currDate0_2, origin = "1970-01-01")
  return(c(currMax, currMax0_2, currDate, currDate0_2))
}

findMin <- function(minData, data, num, dataSurf) {
  if (!missing(dataSurf) && dataSurf[num] > maxData[2]) {
    currMin0_2 <- dataSurf[num]
    currDate0_2 <- myOriginalDataPCAll$DateTime[num]
  }
  else {
    currMin0_2 <- minData[2]
    currDate0_2 <- minData[4]
  }
  if (data[num] > minData[1]) {
    currMin <- data[num]
    currDate <- myOriginalDataPCAll$DateTime[num]
  }
  else {
    currMin <- minData[1]
    currDate <- minData[3]
  }
  return(c(currMin, currMin0_2, currDate, currDate0_2))
}


# Function creates wtr matrix by extracting the temperatures of the top three layers,
# which are 22-33 here (but relabeled 0-2 for clarity). 
findSurface <- function (maxDepth, myOriginalDataPC){
  if (missing(myOriginalDataPC)) {
    myOriginalDataPC = get_var(SimFile,VarName, z_out = 0:(maxDepth))
  }
  myOriginalDataPCSurface <- matrix(NA, nrow=nrow(myOriginalDataPC), ncol=4)
  colnames(myOriginalDataPCSurface) <- c("DateTime", "wtr_2", "wtr_1", "wtr_0")
  myOriginalDataPCSurface[,1] = myOriginalDataPC$DateTime
  data = myOriginalDataPC[2,2]
  for (j in 1:nrow(myOriginalDataPC)) {
    found <- FALSE
    k <- ncol(myOriginalDataPC)
    while (k >= 2 && found == FALSE) {
      if (is.na(myOriginalDataPC[j,k])) {
        found <- TRUE
        l <- k
        while (l >= 2) {
          if (!is.na(myOriginalDataPC[j,l])) {
            thisRow <- c(myOriginalDataPC[j,(l-2)], myOriginalDataPC[j,(l-1)], myOriginalDataPC[j,(l)])
            myOriginalDataPCSurface[j,2:4] <- thisRow
            break
          }
          l = l - 1
        }
      }
      k = k - 1
    }
    if (found == FALSE) { 
      thisRow <- c(myOriginalDataPC[j, maxDepth-2], myOriginalDataPC[j,maxDepth-1], myOriginalDataPC[j,maxDepth])
      myOriginalDataPCSurface[j,2:4] <- thisRow
    }
  }
  return (myOriginalDataPCSurface) 
}

# Separates the bottom 33 meters of lake depth for use in calculations. 
findBottom <- function (Data){
  if (missing(Data)) {
    myOriginalDataPC = get_var(SimFile,VarName, z_out = 0:33)
  }
  else {
    myOriginalDataPC = Data
  }
  myOriginalDataPCBottom <- matrix(NA, nrow=nrow(myOriginalDataPC), ncol=35)
  cols <- 0:33
  sortcols <- sort(cols, decreasing = TRUE)
  colnamers <- c("DateTime", paste0("wtr_", sortcols))
  colnames(myOriginalDataPCBottom) <- c("DateTime", paste0("wtr_", sortcols))
  for (j in 1:35){
    thisColumn <- c(myOriginalDataPC[,j])
    myOriginalDataPCBottom[,j] <- thisColumn
  }
  return (myOriginalDataPCBottom)
}


# Separates the bottom 2 meters of lake depth for sediment evaluation 
findBottomSed <- function (Data){
  if (missing(Data)) {
    myOriginalDataPC = get_var(SimFile,VarName, z_out = 0:33)
  }
  else {
    myOriginalDataPC = Data
  }
  myOriginalDataPCBottom <- matrix(NA, nrow=nrow(myOriginalDataPC), ncol=4)
  cols <- 0:2
  sortcols <- sort(cols, decreasing = TRUE)
  colnamers <- c("DateTime", paste0("wtr_", sortcols))
  colnames(myOriginalDataPCBottom) <- c("DateTime", paste0("wtr_", sortcols))
  for (j in 1:4){
    thisColumn <- c(myOriginalDataPC[,j])
    myOriginalDataPCBottom[,j] <- thisColumn
  }
  return (myOriginalDataPCBottom)
}

# Load NETCDF file
if(file.exists(SimFile)){
  Message = paste('Simulation successfully loaded', sep="")
  print(Message)
  for (i in 1:length(VarsToAnalyze)){
    # ExperimentPC
    VarName = VarsToAnalyze[i]
    print('==============================================')
    print(paste('Analysis of ', VarName))
    print('==============================================')
    if(VarName=="temp"){
      myOriginalDataPC = get_var(SimFile,VarName)
      maxDepth <- findLakeDepth(myOriginalDataPC)
      Surface <- findSurface(maxDepth)
      myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0 : maxDepth)
      Bottom <- findBottom(myOriginalDataPCAll)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1 
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateTemp <- matrix(data = NA, nrow = length(datesNew), ncol = 11)
      for(j in 1:length(dates)){ # Date and time measurements are the same for all lengths
        temp1[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35]))))#pull out only the 0-33 m intervals for each timestep and take avg
        temp2[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp41[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4]))))#pull out only the 0-2 m interval and take avg
        temp42[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp2, j, temp42)
        if (dates[j] != currYear || j == length(dates)) {
          myOriginalDataNew <- changeMyOriginalData(currYear, myOriginalDataPCAll)
          maxDepthY <- findLakeDepth(myOriginalDataNew)
          SurfaceY <- findSurface(maxDepthY, myOriginalDataNew)
          myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0 : 33)
          timeData <- changeMyOriginalData(currYear, myOriginalDataPCAll)
          BottomY <- findBottom(timeData)
          dataByDateTemp[col, 1] <- assign(paste0('mean_mean_temp', datesNew[col]), mean(na.exclude((temp1[front:k]))))
          dataByDateTemp[col, 2] <- assign(paste0('max_max_temp', datesNew[col]), max(na.exclude(temp2[front:k])))
          dataByDateTemp[col, 3] <- maxData[3]
          dataByDateTemp[col, 4] <- assign(paste0('mean_surface_temp', datesNew[col]), mean(na.exclude(SurfaceY[,"wtr_0"])))
          dataByDateTemp[col, 5] <- assign(paste0('max_surface_temp', datesNew[col]), mean(na.exclude(SurfaceY[,"wtr_0"])))
          dataByDateTemp[col, 6] <- assign(paste0('mean_bottom_temp', datesNew[col]), mean(na.exclude(BottomY[,"wtr_33"])))
          dataByDateTemp[col, 7] <- assign(paste0('max_bottom_temp', datesNew[col]), max(na.exclude(BottomY[,"wtr_33"])))
          dataByDateTemp[col, 8] <- assign(paste0('min_bottom_temp', datesNew[col]), min(na.exclude(BottomY[,"wtr_33"])))
          dataByDateTemp[col, 9] <- assign(paste0('mean_0_2_temp', datesNew[col]), mean(na.exclude(temp41[front:k])))
          dataByDateTemp[col, 10] <- assign(paste0('max_0_2_temp', datesNew[col]), max(na.exclude(temp42[front:k])))
          dataByDateTemp[col, 11] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp2, j, temp42)
        }
        k = k + 1
      }
      mean_mean_temp<-mean(na.exclude(temp1)) #mean temp of entire water column
      max_max_temp<-max(na.exclude(temp2)) #max TN of entire water column 
      mean_surface_temp<-mean(na.exclude(Surface[,"wtr_0"])) #mean temp of surface 0m
      max_surface_temp<-max(na.exclude(Surface[,"wtr_0"])) #max temp of surface 0m
      mean_bottom_temp<-mean(na.exclude(Bottom[,"wtr_33"]))
      max_bottom_temp<-max(na.exclude(Bottom[,"wtr_33"]))
      min_bottom_temp<-min(na.exclude(Bottom[,"wtr_33"])) #min temp of bottom 33 m
      mean_0_2_temp<-mean(na.exclude(temp41)) #mean temp from 0-2 m 
      max_0_2_temp<-max(na.exclude(temp42)) #max temp from 0-2 m
      colnames(dataByDateTemp) <- c('mean_mean_temp', 'max_max_temp', 'date_max_max_temp', 'mean_surface_temp', 'max_surface_temp', 'mean_bottom_temp', 'max_bottom_temp', 'min_bottom_temp', 'mean_0_2_temp', 'max_0_2_temp', 'date_max_0_2_temp') # Fix col names
      rownames(dataByDateTemp) <- datesNew 
    }
    if(VarName=="OXY_oxy"){
      myOriginalDataPCDepths = myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0:33)
      Data <- findBottom(myOriginalDataPCDepths)
      Surface <- findSurface(maxDepth)
      Sed <- findBottomSed()
      minData <- resetVars(c(100), 1, c(100))
      mean_bottom_oxy<-mean(na.exclude(Data[,"wtr_33"]))*0.032 #mean oxygen of bottom 33 m in mg/L
      min_bottom_oxy<-min(na.exclude(Data[,"wtr_33"]))*0.032 #min oxygen of bottom 33 m in mg/L
      hypoxic_length<-(length(which(Data[,"wtr_33"]*.032<2)))# of time steps at 33 m with oxygen below 2 mg/L
      anoxic_length<-(length(which(Data[,"wtr_33"]*.032<1)))#of time steps at 33 m with oxygen below 1 mg/L
      # mean_sed_oxy <- mean(na.exclude) Tomorrow morn finish & fix.
      dataByDateOxy <- matrix(data = NA, nrow = length(datesNew), ncol = 4)
      for (i in 1:length(datesNew)) { 
        timeData <- changeMyOriginalData(datesNew[i], Data) 
        dataByDateOxy[i, 1] <- assign(paste0('min_bottom_oxy', datesNew[i]), min(na.exclude(timeData[,"wtr_33"]))*0.032) #all in mg/L
        dataByDateOxy[i, 2] <- assign(paste0('mean_bottom_oxy', datesNew[i]), mean(na.exclude(timeData[,"wtr_33"]))*0.032)
        dataByDateOxy[i, 3] <- assign(paste0('hypoxic_length', datesNew[i]), (length(which(timeData[,"wtr_33"]*.032<2))))
        dataByDateOxy[i, 4] <- assign(paste0('anoxic_length', datesNew[i]), (length(which(timeData[,"wtr_33"]*.032<1))))
        minData <- resetVars(na.exclude(timeData[,"wtr_33"]), i)
      }
      colnames(dataByDateOxy) <- c('min_bottom_oxy', 'mean_bottom_oxy', 'hypoxic_length', 'anoxic_length')
      rownames(dataByDateOxy) <- datesNew 
    }
    if(VarName=="OXY_oxy"){ # Additional oxygen metrics to complement what's there
      myOriginalDataPCDepths = myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0:33)
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom(myOriginalDataPCDepths)
      Sed <- findBottomSed(myOriginalDataPCDepths)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      maxData2 <- resetVars(c(0), 1, c(0))
      dataByDateOxy2 <- matrix(data = NA, nrow = length(datesNew), ncol = 9)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp81[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp82[j]<-(max(na.exclude(Bottom[j,2:35]))) #max observed in water column
        temp83[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) #mean in bottom 2 m
        temp84[j]<-(max(na.exclude(Sed[j,2:3]))) #max in bottom 2 m
        temp85[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp87[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp82, j, temp87)
        maxData2 <- findMax(maxData2, temp84, j)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateOxy2[col, 1] <- assign(paste0('mean_mean_oxy', datesNew[col]), mean(na.exclude((temp81[front:k])))*0.032)
          dataByDateOxy2[col, 2] <- assign(paste0('max_max_oxy', datesNew[col]), max(na.exclude(temp82[front:k]))*0.032)
          dataByDateOxy2[col, 3] <- maxData[3]
          dataByDateOxy2[col, 4] <- assign(paste0('mean_0_2_oxy', datesNew[col]), mean(na.exclude(temp85[front:k]))*0.032)
          dataByDateOxy2[col, 5] <- assign(paste0('max_0_2_oxy', datesNew[col]), max(na.exclude(temp87[front:k]))*0.032)
          dataByDateOxy2[col, 6] <- maxData[4] 
          dataByDateOxy2[col, 7] <- assign(paste0('mean_sed_oxy', datesNew[col]), mean(na.exclude(temp83[front:k]))*0.032)
          dataByDateOxy2[col, 8] <- assign(paste0('max_sed_oxy', datesNew[col]), max(na.exclude(temp84[front:k]))*0.032)
          dataByDateOxy2[col, 9] <- maxData2[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp82, j, temp87)
          maxData2 <- resetVars(temp84, j)
        }
        k = k + 1
      }
      mean_mean_oxy2<-mean(na.exclude(temp82)*0.032) #mean OXY of entire water column in mmol/m3
      max_max_oxy2<-max(na.exclude(temp83)*0.032) #max OXY of entire water column	in mmol/m3
      mean_0_2_oxy2<-mean(na.exclude(temp85)*0.032) #mean OXY from 0-2 m in mmol/m3
      max_0_2_oxy2<-max(na.exclude(temp87)*0.032) #max OXY from 0-2 mg in mmol/m3
      mean_sed_oxy2<-mean(na.exclude(temp83)*0.032) #mean OXY from bottom 0-2 in mmol/m3
      max_sed_oxy2<-max(na.exclude(temp84)*0.032) #max OXY from bottom 0-2 in mmol/m3
      colnames(dataByDateOxy2) <- c('mean_mean_oxy2', 'max_max_oxy2', 'date_max_oxy2', 'mean_0_2_oxy2', 'max_0_2_oxy2', 'date_max_surface_oxy2', 'mean_sed_oxy2', 'max_sed_oxy2', 'date_max_sed_oxy2')
      rownames(dataByDateOxy2) <- datesNew 
    }
    if(VarName=="TOT_tn"){
      myOriginalDataPCDepths = myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0:33)
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom(myOriginalDataPCDepths)
      Sed <- findBottomSed(myOriginalDataPCDepths)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      maxData2 <- resetVars(c(0), 1, c(0))
      dataByDateTOTTn <- matrix(data = NA, nrow = length(datesNew), ncol = 9)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp88[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp89[j]<-(max(na.exclude(Bottom[j,2:35]))) #max observed in water column
        temp90[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) #mean in bottom 2 m
        temp91[j]<-(max(na.exclude(Sed[j,2:3]))) #max in bottom 2 m
        temp92[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp93[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp89, j, temp92)
        maxData2 <- findMax(maxData2, temp91, j)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateTOTTn[col, 1] <- assign(paste0('mean_mean_TN', datesNew[col]), mean(na.exclude((temp88[front:k])))*0.014)
          dataByDateTOTTn[col, 2] <- assign(paste0('max_max_TN', datesNew[col]), max(na.exclude(temp89[front:k]))*0.014)
          dataByDateTOTTn[col, 3] <- maxData[3]
          dataByDateTOTTn[col, 4] <- assign(paste0('mean_0_2_TN', datesNew[col]), mean(na.exclude(temp92[front:k]))*0.014)
          dataByDateTOTTn[col, 5] <- assign(paste0('max_0_2_TN', datesNew[col]), max(na.exclude(temp93[front:k]))*0.014)
          dataByDateTOTTn[col, 6] <- maxData[4] 
          dataByDateTOTTn[col, 7] <- assign(paste0('mean_sed_TN', datesNew[col]), mean(na.exclude(temp90[front:k]))*0.014)
          dataByDateTOTTn[col, 8] <- assign(paste0('max_sed_TN', datesNew[col]), max(na.exclude(temp91[front:k]))*0.014)
          dataByDateTOTTn[col, 9] <- maxData2[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp89, j, temp92)
          maxData2 <- resetVars(temp91, j)
        }
        k = k + 1
      }
      mean_mean_TN<-mean(na.exclude(temp88))*0.014 #mean TN of entire water column in mg/L
      max_max_TN<-max(na.exclude(temp89))*0.014 #max TN of entire water column	in mg/L
      mean_0_2_TN<-mean(na.exclude(temp92))*0.014 #mean TN from 0-2 m in mg/L
      max_0_2_TN<-max(na.exclude(temp92))*0.014 #max TN from 0-2 mg in mg/L
      mean_sed_TN<-mean(na.exclude(temp90))*0.014 #mean TN from bottom 0-2 in mg/L
      max_sed_TN<-max(na.exclude(temp91))*0.014 #max TN from bottom 0-2 in mg/L
      colnames(dataByDateTOTTn) <- c('mean_mean_TN', 'max_max_TN', 'date_max_TN', 'mean_0_2_TN', 'max_0_2_TN', 'date_max_surface_TN', 'mean_sed_TN', 'max_sed_TN', 'date_max_sed_TN')
      rownames(dataByDateTOTTn) <- datesNew 
    }
    if(VarName=="TOT_tp"){
      myOriginalDataPCDepths = myOriginalDataPCAll = get_var(SimFile,VarName, z_out = 0:33)
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom(myOriginalDataPCDepths)
      Sed <- findBottomSed(myOriginalDataPCDepths)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      maxData2 <- resetVars(c(0), 1, c(0))
      dataByDateTOTTp <- matrix(data = NA, nrow = length(datesNew), ncol = 9)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp3[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp4[j]<-(max(na.exclude(Bottom[j,2:35]))) #max observed in water column
        temp49[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) #mean in bottom 2 m
        temp50[j]<-(max(na.exclude(Sed[j,2:3]))) #max in bottom 2 m
        temp5[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp6[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp4, j, temp6)
        maxData2 <- findMax(maxData2, temp50, j)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateTOTTp[col, 1] <- assign(paste0('mean_mean_TP', datesNew[col]), mean(na.exclude((temp3[front:k])))*0.031)
          dataByDateTOTTp[col, 2] <- assign(paste0('max_max_TP', datesNew[col]), max(na.exclude(temp4[front:k]))*0.031)
          dataByDateTOTTp[col, 3] <- maxData[3]
          dataByDateTOTTp[col, 4] <- assign(paste0('mean_0_2_TP', datesNew[col]), mean(na.exclude(temp5[front:k]))*0.031)
          dataByDateTOTTp[col, 5] <- assign(paste0('max_0_2_TP', datesNew[col]), max(na.exclude(temp6[front:k]))*0.031)
          dataByDateTOTTp[col, 6] <- maxData[4] 
          dataByDateTOTTp[col, 7] <- assign(paste0('mean_sed_TP', datesNew[col]), mean(na.exclude(temp49[front:k]))*0.031)
          dataByDateTOTTp[col, 8] <- assign(paste0('max_sed_TP', datesNew[col]), max(na.exclude(temp50[front:k]))*0.031)
          dataByDateTOTTp[col, 9] <- maxData2[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp4, j, temp6)
          maxData2 <- resetVars(temp50, j)
        }
        k = k + 1
      }
      mean_mean_TP<-mean(na.exclude(temp3))*0.031 #mean TP of entire water column in mg/L
      max_max_TP<-max(na.exclude(temp4))*0.031 #max TP of entire water column	in mg/L
      mean_0_2_TP<-mean(na.exclude(temp5))*0.031 #mean TP from 0-2 m in mg/L
      max_0_2_TP<-max(na.exclude(temp6))*0.031 #max TP from 0-2 mg in mg/L
      mean_sed_TP<-mean(na.exclude(temp49))*0.031 #mean TP from bottom 0-2 in mg/L
      max_sed_TP<-max(na.exclude(temp50))*0.031 #max TP from bottom 0-2 in mg/L
      colnames(dataByDateTOTTp) <- c('mean_mean_TP', 'max_max_TP', 'date_max_TP', 'mean_0_2_TP', 'max_0_2_TP', 'date_max_surface_TP', 'mean_sed_TP', 'max_sed_TP', 'date_max_sed_TP')
      rownames(dataByDateTOTTp) <- datesNew 
    }
    if(VarName=="PHY_TPHYS"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      maxData <- resetVars(c(0), 1, c(0))
      k <- 1
      col <- 1
      front <- 1
      dataByDateTPHYS <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp11[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp12[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp13[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp14[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp12, j, temp14)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateTPHYS[col, 1] <- assign(paste0('mean_mean_TPHYS', datesNew[col]), mean(na.exclude((temp11[front:k]))))
          dataByDateTPHYS[col, 2] <- assign(paste0('max_max_TPHYS', datesNew[col]), max(na.exclude(temp12[front:k])))
          dataByDateTPHYS[col, 3] <- maxData[3]
          dataByDateTPHYS[col, 4] <- assign(paste0('mean_0_2_TPHYS', datesNew[col]), mean(na.exclude(temp13[front:k])))
          dataByDateTPHYS[col, 5] <- assign(paste0('max_0_2_TPHYS', datesNew[col]), max(na.exclude(temp14[front:k])))
          dataByDateTPHYS[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp12, j, temp14)
        }
        k = k + 1
      }
      mean_mean_totalphytos<-mean(na.exclude(temp11)) #mean total phytos of entire water column in mmol C/m3
      max_max_totalphytos<-max(na.exclude(temp12)) #max total phytos of entire water column in mmol C/m3
      mean_0_2_totalphytos<-mean(na.exclude(temp13)) #mean total phytos from 0-2 m in mg/L
      max_0_2_totalphytos<-max(na.exclude(temp14)) #max total phytos from 0-2 mg in mg/L
      colnames(dataByDateTPHYS) <- c('mean_mean_TPHYS', 'max_max_TPHYS', 'date_max_TPHYS', 'mean_0_2_TPHYS', 'max_0_2_TPHYS', 'date_max_surface_TPHYS')
      rownames(dataByDateTPHYS) <- datesNew 
    }
    if(VarName=="PHY_TCHLA"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      maxData <- resetVars(c(0), 1, c(0))
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      dataByDateTCHLA <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp15[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp16[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp17[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp18[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp16, j, temp18)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateTCHLA[col, 1] <- assign(paste0('mean_mean_TCHLA', datesNew[col]), mean(na.exclude((temp15[front:k]))))
          dataByDateTCHLA[col, 2] <- assign(paste0('max_max_TCHLA', datesNew[col]), max(na.exclude(temp16[front:k])))
          dataByDateTCHLA[col, 3] <- maxData[3]
          dataByDateTCHLA[col, 4] <- assign(paste0('mean_0_2_TCHLA', datesNew[col]), mean(na.exclude(temp17[front:k])))
          dataByDateTCHLA[col, 5] <- assign(paste0('max_0_2_TCHLA', datesNew[col]), max(na.exclude(temp18[front:k])))
          dataByDateTCHLA[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp16, j, temp18)
        }
        k = k + 1
      }
      mean_mean_totalchla<-mean(na.exclude(temp15)) #mean total chla of entire water column in ug/L
      max_max_totalchla<-max(na.exclude(temp16)) #max total chla of entire water column in ug/L
      mean_0_2_totalchla<-mean(na.exclude(temp17)) #mean total chla from 0-2 m in mg/L
      max_0_2_totalchla<-max(na.exclude(temp18)) #max total chla from 0-2 mg in mg/L
      colnames(dataByDateTCHLA) <- c('mean_mean_TCHLA', 'max_max_TCHLA', 'date_max_TCHLA', 'mean_0_2_TCHLA', 'max_0_2_TCHLA', 'data_max_surface_TCHLA')
      rownames(dataByDateTCHLA) <- datesNew 
    }
    if(VarName=="PHY_CYANOPCH1"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      dataByDatePCH1 <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp19[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp20[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp21[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp22[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp20, j, temp22)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDatePCH1[col, 1] <- assign(paste0('mean_mean_nonNfixingcyanos', datesNew[col]), mean(na.exclude((temp19[front:k]))))
          dataByDatePCH1[col, 2] <- assign(paste0('max_max_nonNfixingcyanos', datesNew[col]), max(na.exclude(temp20[front:k])))
          dataByDatePCH1[col, 3] <- maxData[3]
          dataByDatePCH1[col, 4] <- assign(paste0('mean_0_2_nonNfixingcyanos', datesNew[col]), mean(na.exclude(temp21[front:k])))
          dataByDatePCH1[col, 5] <- assign(paste0('max_0_2_nonNfixingcyanos', datesNew[col]), max(na.exclude(temp22[front:k])))
          dataByDatePCH1[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp20, j, temp22)
        }
        k = k + 1
      }
      mean_mean_nonNfixingcyanos<-mean(na.exclude(temp19)) #mean nonNfixingcyanos of entire water column
      max_max_nonNfixingcyanos<-max(na.exclude(temp20)) #max nonNfixingcyanos of entire water column	
      mean_0_2_nonNfixingcyanos<-mean(na.exclude(temp21)) #mean nonNfixingcyanos from 0-2 m in mg/L
      max_0_2_nonNfixingcyanos<-max(na.exclude(temp22)) #max nonNfixingcyanos from 0-2 mg in mg/L
      colnames(dataByDatePCH1) <- c('mean_mean_nonNfixingcyanos', 'max_max_nonNfixingcyanos', 'date_max_max_nonNfixingcyanos', 'mean_0_2_nonNfixingcyanos', 'max_0_2_nonNfixingcyanos', 'date_max_0_2_nonNfixingcyanos')
      rownames(dataByDatePCH1) <- datesNew 
    }
    if(VarName=="PHY_CYANONPCH2"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      dataByDatePCH2 <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp23[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp24[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp25[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp26[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp24, j, temp26)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDatePCH2[col, 1] <- assign(paste0('mean_mean_Nfixingcyanos', datesNew[col]), mean(na.exclude((temp23[front:k]))))
          dataByDatePCH2[col, 2] <- assign(paste0('max_max_Nfixingcyanos', datesNew[col]), max(na.exclude(temp24[front:k])))
          dataByDatePCH2[col, 3] <- maxData[3]
          dataByDatePCH2[col, 4] <- assign(paste0('mean_0_2_Nfixingcyanos', datesNew[col]), mean(na.exclude(temp25[front:k])))
          dataByDatePCH2[col, 5] <- assign(paste0('max_0_2_Nfixingcyanos', datesNew[col]), max(na.exclude(temp26[front:k])))
          dataByDatePCH2[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp24, j, temp26)
        }
        k = k + 1
      }
      mean_mean_Nfixingcyanos<-mean(na.exclude(temp23)) #mean Nfixingcyanos of entire water column
      max_max_Nfixingcyanos<-max(na.exclude(temp24)) #max Nfixingcyanos of entire water column	
      mean_0_2_Nfixingcyanos<-mean(na.exclude(temp25)) #mean Nfixingcyanos from 0-2 m in mg/L
      max_0_2_Nfixingcyanos<-max(na.exclude(temp26)) #max Nfixingcyanos from 0-2 mg in mg/L
      colnames(dataByDatePCH2) <- c('mean_mean_Nfixingcyanos', 'max_max_Nfixingcyanos', 'date_max_max_Nfixingcyanos', 'mean_0_2_Nfixingcyanos', 'max_0_2_Nfixingcyanos', 'date_max_0_2_Nfixingcyanos')
      rownames(dataByDatePCH2) <- datesNew 
    }
    if(VarName=="PHY_CHLOROPCH3"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      dataByDatePCH3 <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp27[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp28[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp29[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp30[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp28, j, temp30)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDatePCH3[col, 1] <- assign(paste0('mean_mean_chlorophytes', datesNew[col]), mean(na.exclude((temp27[front:k]))))
          dataByDatePCH3[col, 2] <- assign(paste0('max_max_chlorophytes', datesNew[col]), max(na.exclude(temp28[front:k])))
          dataByDatePCH3[col, 3] <- maxData[3]
          dataByDatePCH3[col, 4] <- assign(paste0('mean_0_2_chlorophytes', datesNew[col]), mean(na.exclude(temp29[front:k])))
          dataByDatePCH3[col, 5] <- assign(paste0('max_0_2_chlorophytes', datesNew[col]), max(na.exclude(temp30[front:k])))
          dataByDatePCH3[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp28, j, temp30)
        }
        k = k + 1
      }
      mean_mean_chlorophytes<-mean(na.exclude(temp27)) #mean nonNfixingcyanos of entire water column
      max_max_chlorophytes<-max(na.exclude(temp28)) #max nonNfixingcyanos of entire water column	
      mean_0_2_chlorophytes<-mean(na.exclude(temp29)) #mean nonNfixingcyanos from 0-2 m in mg/L
      max_0_2_chlorophytes<-max(na.exclude(temp30)) #max nonNfixingcyanos from 0-2 mg in mg/L
      colnames(dataByDatePCH3) <- c('mean_mean_chlorophytes', 'max_max_chlorophytes', 'date_max_max_chlorophytes', 'mean_0_2_chlorophytes', 'max_0_2_chlorophytes', 'date_max_0_2_chlorophytes')
      rownames(dataByDatePCH3) <- datesNew
    }
    if(VarName=="PHY_DIATOMPCH4"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      dataByDatePCH4 <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp31[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp32[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp33[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp34[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp32, j, temp34)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDatePCH4[col, 1] <- assign(paste0('mean_mean_diatoms', datesNew[col]), mean(na.exclude((temp31[front:k]))))
          dataByDatePCH4[col, 2] <- assign(paste0('max_max_diatoms', datesNew[col]), max(na.exclude(temp32[front:k])))
          dataByDatePCH4[col, 3] <- maxData[3]
          dataByDatePCH4[col, 4] <- assign(paste0('mean_0_2_diatoms', datesNew[col]), mean(na.exclude(temp33[front:k])))
          dataByDatePCH4[col, 5] <- assign(paste0('max_0_2_diatoms', datesNew[col]), max(na.exclude(temp34[front:k])))
          dataByDatePCH4[col, 6] <- maxData[4]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp32, j, temp34)
        }
        k = k + 1
      }
      mean_mean_diatoms<-mean(na.exclude(temp31)) #mean diatoms of entire water column
      max_max_diatoms<-max(na.exclude(temp32)) #max diatoms of entire water column	
      mean_0_2_diatoms<-mean(na.exclude(temp33)) #mean diatoms from 0-2 m in mg/L
      max_0_2_diatoms<-max(na.exclude(temp34)) #max diatoms from 0-2 mg in mg/L
      colnames(dataByDatePCH4) <- c('mean_mean_diatoms', 'max_max_diatoms', 'date_max_max_diatoms', 'mean_0_2_diatoms', 'max_0_2_diatoms', 'date_max_0_2_diatoms')
      rownames(dataByDatePCH4) <- datesNew
    }
    if(VarName=="PHY_PPR"){
      Surface <- findSurface(maxDepth)
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1
      maxData <- resetVars(c(0), 1, c(0))
      dataByDatePPR <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp35[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp36[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp37[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) #pull out only the 0-2 m interval and take avg
        temp38[j]<-(max(na.exclude(Surface[j,2:4]))) #max observed in 0-2 m
        maxData <- findMax(maxData, temp36, j, temp38)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDatePPR[col, 1] <- assign(paste0('mean_mean_GPP', datesNew[col]), mean(na.exclude((temp35[front:k]))))
          dataByDatePPR[col, 2] <- assign(paste0('max_max_GPP', datesNew[col]), max(na.exclude(temp36[front:k])))
          dataByDatePPR[col, 3] <- maxData[3]
          dataByDatePPR[col, 4] <- assign(paste0('mean_0_2_GPP', datesNew[col]), mean(na.exclude(temp37[front:k])))
          dataByDatePPR[col, 5] <- assign(paste0('max_0_2_GPP', datesNew[col]), max(na.exclude(temp38[front:k])))
          dataByDatePPR[col, 6] <- maxData[4]
          col = col + 1
          maxData <- resetVars(temp36, j, temp38)
          front = k + 1
          currYear <- dates[j]
        }
        k = k + 1
      }
      mean_mean_GPP<-mean(na.exclude(temp35)) #mean GPP of entire water column
      max_max_GPP<-max(na.exclude(temp36)) #max GPP of entire water column	
      mean_0_2_GPP<-mean(na.exclude(temp37)) #mean GPP from 0-2 m in mg/L
      max_0_2_GPP<-max(na.exclude(temp38)) #max GPP from 0-2 mg in mg/L
      colnames(dataByDatePPR) <- c('mean_mean_GPP', 'max_max_GPP', 'date_max_max_GPP', 'mean_0_2_GPP', 'max_0_2_GPP', 'date_max_0_2_GPP')
      rownames(dataByDatePPR) <- datesNew
    }
    if(VarName=="TOT_tss"){
      Bottom <- findBottom()
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateTSS <- matrix(data = NA, nrow = length(datesNew), ncol = 3)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp39[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp40[j]<-(max(na.exclude(Bottom[j,2:35])))
        maxData <- findMax(maxData, temp40, j)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateTSS[col, 1] <- assign(paste0('mean_mean_TSS', datesNew[col]), mean(na.exclude((temp39[front:k]))))
          dataByDateTSS[col, 2] <- assign(paste0('max_max_TSS', datesNew[col]), max(na.exclude(temp40[front:k])))
          dataByDateTSS[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp40, j)
        }
        k = k + 1
      }
      mean_mean_TSS<-mean(na.exclude(temp39)) #mean TSS of entire water column
      max_max_TSS<-max(na.exclude(temp40)) #max TSS of entire water column
      colnames(dataByDateTSS) <- c('mean_mean_TSS', 'max_max_TSS', 'date_max_max_TSS')
      rownames(dataByDateTSS) <- datesNew 
    }
    if (VarName == 'hice') { 
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      dataByDateIce <- matrix(data = NA, nrow = length(datesNew), ncol = 3)
      ice <- get_ice(SimFile)
      iceOnDate <- NA
      iceOffDate <- NA
      isIceOn <- FALSE
      days_of_ice <- 0
      for(j in 1:length(myOriginalDataPC$DateTime)){
        maxData <- findMax(maxData, temp40, j)
        if (isIceOn) {
          days_of_ice = days_of_ice + 0.25
          if (ice[j,2] == 0) {
            isIceOn = FALSE
            iceOffDate <- myOriginalDataPC$DateTime[j]
          }
        }
        else {
          if (ice[j,2] != 0) {
            isIceOn = TRUE
            iceOnDate <- myOriginalDataPC$DateTime[j]
          }
        }
        if ((j > 1 && months[j] == 10 && dates[j] != currYear) || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateIce[col, 1] <- assign(paste0('ice_on_', datesNew[col]), iceOnDate)
          dataByDateIce[col, 2] <- assign(paste0('ice_off_', datesNew[col]), iceOffDate)
          dataByDateIce[col, 3] <- assign(paste0('ice_duration_', datesNew[col]), days_of_ice)
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          iceOffDate <- NA
          iceOnData <- NA
          days_of_ice <- 0
        }
        k = k + 1
      }
      colnames(dataByDateIce) <- c('ice_on', 'ice_off', 'ice_duration')
      rownames(dataByDateIce) <- datesNew 
    }
    if (VarName == "NIT_sed_nit") {
      BottomNIT <- get_var(SimFile,VarName, z_out = 0:33)
      VarName = "NIT_sed_amm"
      BottomAMM <- get_var(SimFile,VarName, z_out = 0:33)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateDINsedflux <- matrix(data = NA, nrow = length(datesNew), ncol = 1)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp41[j]<- (BottomNIT[j,2] + BottomAMM[j,2])#pull out only the 0-33 m intervals for each timestep and take avg
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateDINsedflux[col, 1] <- assign(paste0('mean_mean_DINsedflux', datesNew[col]), mean(na.exclude((temp41[front:k]))))
          col = col + 1
          front = k + 1
          currYear <- dates[j]
        }
        k = k + 1
      }
      mean_mean_DINsedflux<-mean(na.exclude(temp41)) #mean DINsedflux of entire water column
      colnames(dataByDateDINsedflux) <- c('mean_mean_DINsedflux')
      rownames(dataByDateDINsedflux) <- datesNew 
    }
    if(VarName=="PHS_sed_frp"){
      Bottom <- get_var(SimFile,VarName, z_out = 0:33)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateDIPsedflux <- matrix(data = NA, nrow = length(datesNew), ncol = 1)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp42[j]<-Bottom[j,2]
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateDIPsedflux[col, 1] <- assign(paste0('mean_mean_DIPsedflux', datesNew[col]), mean(na.exclude((temp42[front:k]))))
          col = col + 1
          front = k + 1
          currYear <- dates[j]
        }
        k = k + 1
      }
      mean_mean_DIPsedflux<-mean(na.exclude(temp42)) #mean DIPsedflux of entire water column
      colnames(dataByDateDIPsedflux) <- c('mean_mean_DIPsedflux')
      rownames(dataByDateDIPsedflux) <- datesNew 
    }
    if(VarName=="NIT_sed_amm"){
      Bottom <- get_var(SimFile,VarName, z_out = 0:33)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateAMMsedflux <- matrix(data = NA, nrow = length(datesNew), ncol = 1)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp73[j]<-Bottom[j,2]
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateAMMsedflux[col, 1] <- assign(paste0('mean_mean_AMMsedflux', datesNew[col]), mean(na.exclude((temp73[front:k]))))
          col = col + 1
          front = k + 1
          currYear <- dates[j]
        }
        k = k + 1
      }
      mean_mean_AMMsedflux<-mean(na.exclude(temp42)) #mean AMMsedflux of entire water column
      colnames(dataByDateAMMsedflux) <- c('mean_mean_AMMsedflux')
      rownames(dataByDateAMMsedflux) <- datesNew 
    }
    if(VarName=="OXY_sed_oxy"){
      Bottom <- get_var(SimFile,VarName, z_out = 0:33)
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateOXYsedflux <- matrix(data = NA, nrow = length(datesNew), ncol = 1)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp72[j]<-Bottom[j,2]
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateOXYsedflux[col, 1] <- assign(paste0('mean_mean_OXYsedflux', datesNew[col]), mean(na.exclude((temp72[front:k]))))
          col = col + 1
          front = k + 1
          currYear <- dates[j]
        }
        k = k + 1
      }
      mean_mean_OXYsedflux<-mean(na.exclude(temp42)) #mean OXYsedflux of entire water column
      colnames(dataByDateOXYsedflux) <- c('mean_mean_OXYsedflux')
      rownames(dataByDateOXYsedflux) <- datesNew 
    }
    if(VarName=="NIT_nit"){ # Get nitrate fluxes for entire water column yearly.
      Surface <- findSurface(maxDepth)
      Sed <- findBottomSed()
      Bottom <- findBottom()
      VolName <- "Tot_V"
      Volume <- get_var(SimFile,VolName, z_out = 0:2)
      initVol <- Volume[1,2]
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      yearlyLoad <- 0
      maxData <- resetVars(c(0), 1, c(0))
      conversion <- 14 # g/mol of nutrient base 
      dataByDateNIT <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){ 
        temp53[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) * 14 /1000# to get mg / L as N
        temp58[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) * 14 /1000 # to get mg/L / m3 #pull out only the 0-33 m intervals for each timestep and take avg
        temp54[j]<-(max(na.exclude(Bottom[j,2:35]))) * 14 /1000
        temp63[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) * 14 /1000# to get mg/L #pull out only the 0-33 m intervals for each timestep and take avg
        temp64[j]<-(max(na.exclude(Surface[j,2:4]))) * 14 /1000# to get mg/L
        temp65[j]<-(max(na.exclude(Sed[j,2:3]))) * 14 /1000# to get mg/L
        maxData <- findMax(maxData, temp54, j)
        yearlyLoad = yearlyLoad + (temp53[j] * Volume[j, 2])
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateNIT[col, 1] <- assign(paste0('mean_mean_NIT', datesNew[col]), mean(na.exclude((temp58[front:k])))) 
          dataByDateNIT[col, 2] <- assign(paste0('max_max_NIT', datesNew[col]), max(na.exclude(temp54[front:k])))
          dataByDateNIT[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          yearlyLoad = 0
          maxData <- resetVars(temp68, j)
        }
        k = k + 1
      }
      Var <- VarName
      index = 0
      for (i in 1:length(colnames(inflow))) {
        if (colnames(inflow)[i] == Var) {
          index = i
        }
      }
      inflowload <- c(rep(NA, length(inflow$time)))
      outflowload <- c(rep(NA, length(inflow$time)))
      retention <- c(rep(NA, length(inflow$time)))
      nutrientconccounter <- 1
      sum <- 0
      count <- 0
      currYear <- dates[1]
      front <- 1
      for (z in 1:length(inflow$time)) {
        inflowload[z] <- inflow[z, index] * 14 * inflow$FLOW[z] * 89.4 / 1000 # could be time 14
        while (!is.na(days[nutrientconccounter]) && !is.na(uniquedays[z]) && days[nutrientconccounter] == uniquedays[z]) {
          sum = sum + (temp53[nutrientconccounter] * 89.4) # in kg
          nutrientconccounter = nutrientconccounter + 1
          count = count + 1
        }
        outflowload[z] <- (outflow[z, 2] + overflow$FLOW[z]) * (sum/count)
        count = 0
        sum = 0
        retention[z] <- (inflowload[z] - outflowload[z]) / inflowload[z]
        if (!is.na(dates[nutrientconccounter]) && (nutrientconccounter == length(dates) || dates[nutrientconccounter] != currYear)) {
          currYear <- dates[nutrientconccounter-1]  
          dataByDateNIT[currYear - 2009, 4] = assign(paste0('inflow_load_NIT', datesNew[currYear - 2009]), sum(inflowload[front:z]))
          dataByDateNIT[currYear - 2009, 5] = assign(paste0('outflow_load_NIT', datesNew[currYear - 2009]), sum(outflowload[front:z]))
          dataByDateNIT[currYear - 2009, 6] = assign(paste0('retention_NIT', datesNew[currYear - 2009]), (sum(inflowload[front:z]) - sum(outflowload[front:z])) / (sum(inflowload[front:z])))
          front = z
        }
      }
      totalNIT <- data.frame(cbind(data.frame(uniquedays[1:1553]), inflowload, outflowload, retention))
      mean_mean_NIT<-mean(na.exclude(temp58)) #mean NIT of entire water column
      max_max_NIT<-max(na.exclude(temp54)) #max NIT of entire water column
      max_surface_NIT<-max(na.exclude(temp64)) #max NIT of surface waters
      mean_surface_NIT<-mean(na.exclude(temp53)) #mean NIT of surface waters
      max_sed_NIT<-max(na.exclude(temp65)) #max NIT of sediment 0-2 m 
      mean_sed_NIT<-mean(na.exclude(temp63)) #mean NIT of sediment 0-2 m
      total_inload_NIT <- sum(inflowload)
      total_outload_NIT <- sum(outflowload)
      mean_inload_NIT <- mean(inflowload)
      mean_outload_NIT <- mean(outflowload)
      colnames(dataByDateNIT) <- c('mean_mean_NIT', 'max_max_NIT', 'date_max_max_NIT', 'inflow_load_NIT', 'outflow_load_NIT', 'retention_NIT')
      rownames(dataByDateNIT) <- datesNew 
    }
    if(VarName=="NIT_amm"){ # Get ammonium fluxes for entire water column yearly.
      Surface <- findSurface(maxDepth)
      Sed <- findBottomSed()
      Bottom <- findBottom()
      VolName <- "Tot_V"
      Volume <- get_var(SimFile,VolName, z_out = 0:2)
      initVol <- Volume[1,2]
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      yearlyLoad <- 0
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateAMM <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){ 
        temp66[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) * 14 /1000# to get mg / m3 as N
        temp67[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) * 14 /1000 # to get mg/L / m3 #pull out only the 0-33 m intervals for each timestep and take avg
        temp68[j]<-(max(na.exclude(Bottom[j,2:35]))) * 14
        temp69[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) * 14 /1000# to get mg/L #pull out only the 0-33 m intervals for each timestep and take avg
        temp70[j]<-(max(na.exclude(Surface[j,2:4]))) * 14 /1000# to get mg/L
        temp71[j]<-(max(na.exclude(Sed[j,2:3]))) * 14 /1000 # to get mg/L
        maxData <- findMax(maxData, temp68, j)
        yearlyLoad = yearlyLoad + (temp66[j] * Volume[j, 2])
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateAMM[col, 1] <- assign(paste0('mean_mean_AMM', datesNew[col]), mean(na.exclude((temp67[front:k])))) 
          dataByDateAMM[col, 2] <- assign(paste0('max_max_AMM', datesNew[col]), max(na.exclude(temp68[front:k])))
          dataByDateAMM[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          yearlyLoad = 0
          maxData <- resetVars(temp68, j)
        }
        k = k + 1
      }
      Var <- VarName
      index = 0
      for (i in 1:length(colnames(inflow))) {
        if (colnames(inflow)[i] == Var) {
          index = i
        }
      }
      inflowload <- c(rep(NA, length(inflow$time)))
      outflowload <- c(rep(NA, length(inflow$time)))
      retention <- c(rep(NA, length(inflow$time)))
      nutrientconccounter <- 1
      sum <- 0
      count <- 0
      currYear <- dates[1]
      front <- 1
      for (z in 1:length(inflow$time)) {
        inflowload[z] <- inflow[z, index] * 14 * inflow$FLOW[z] * 89.4 / 1000 # could be time 14
        while (!is.na(days[nutrientconccounter]) && !is.na(uniquedays[z]) && days[nutrientconccounter] == uniquedays[z]) {
          sum = sum + (temp66[nutrientconccounter] * 89.4) # in kg - you divided twice
          nutrientconccounter = nutrientconccounter + 1
          count = count + 1
        }
        outflowload[z] <- (outflow[z, 2] + overflow$FLOW[z]) * (sum/count)
        count = 0
        sum = 0
        retention[z] <- (inflowload[z] - outflowload[z]) / inflowload[z]
        if (!is.na(dates[nutrientconccounter]) && (nutrientconccounter == length(dates) || dates[nutrientconccounter] != currYear)) {
          currYear <- dates[nutrientconccounter-1]  
          dataByDateAMM[currYear - 2009, 4] = assign(paste0('inflow_load_AMM', datesNew[currYear - 2009]), sum(inflowload[front:z]))
          dataByDateAMM[currYear - 2009, 5] = assign(paste0('outflow_load_AMM', datesNew[currYear - 2009]), sum(outflowload[front:z]))
          dataByDateAMM[currYear - 2009, 6] = assign(paste0('retention_AMM', datesNew[currYear - 2009]), (sum(inflowload[front:z]) - sum(outflowload[front:z])) / (sum(inflowload[front:z])))
          front = z
        }
      }
      totalAMM <- data.frame(cbind(data.frame(uniquedays[1:1553]), inflowload, outflowload, retention))
      mean_mean_AMM<-mean(na.exclude(temp67)) #mean AMM of entire water column
      max_max_AMM<-max(na.exclude(temp68)) #max AMM of entire water column
      max_surface_AMM<-max(na.exclude(temp70)) #max AMM of surface waters
      mean_surface_AMM<-mean(na.exclude(temp66)) #mean AMM of surface waters
      max_sed_AMM<-max(na.exclude(temp71)) #max AMM of sediment 0-2 m 
      mean_sed_AMM<-mean(na.exclude(temp69)) #mean AMM of sediment 0-2 m
      total_inload_AMM <- sum(inflowload)
      total_outload_AMM <- sum(outflowload)
      mean_inload_AMM <- mean(inflowload)
      mean_outload_AMM <- mean(outflowload)
      colnames(dataByDateAMM) <- c('mean_mean_AMM', 'max_max_AMM', 'date_max_max_AMM', 'inflow_load_AMM', 'outflow_load_AMM', 'retention_AMM')
      rownames(dataByDateAMM) <- datesNew 
    }
    if(VarName=="PHS_frp"){ # Get phosphorus fluxes for entire water column yearly.
      Bottom <- findBottom()
      Surface <- findSurface(maxDepth)
      sed <- findBottomSed()
      VolName <- "Tot_V"
      Volume <- get_var(SimFile,VolName, z_out = 0:2)
      initVol <- Volume[1,2]
      currYear <- dates[1]
      k <- 1
      col <- 1
      front <- 1  
      maxData <- resetVars(c(0), 1, c(0))
      dataByDateFRP <- matrix(data = NA, nrow = length(datesNew), ncol = 6)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp55[j]<-(sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4])))) * 30.97 /1000 # to get mg/L by P
        temp59[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) * 30.97 /1000 # to get mg #pull out only the 0-33 m intervals for each timestep and take avg
        temp60[j]<-(sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4])))) * 30.97 /1000# to get mg/L #pull out only the 0-33 m intervals for each timestep and take avg
        temp56[j]<-(max(na.exclude(Bottom[j,2:35]))) * 30.97 /1000# to get mg/L
        temp61[j]<-(max(na.exclude(Surface[j,2:4]))) * 30.97 /1000# to get mg/L
        temp62[j]<-(max(na.exclude(Sed[j,2:3]))) * 30.97 /1000 # to get mg/L
        yearlyLoad = yearlyLoad + (temp55[j] * Volume[j, 2])
        maxData <- findMax(maxData, temp56, j)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1]  
          dataByDateFRP[col, 1] <- assign(paste0('mean_mean_FRP', datesNew[col]), mean(na.exclude((temp59[front:k]))))
          dataByDateFRP[col, 2] <- assign(paste0('max_max_FRP', datesNew[col]), max(na.exclude(temp56[front:k])))
          dataByDateFRP[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          yearlyLoad = 0
          maxData <- resetVars(temp68, j)
        }
        k = k + 1
      }
      Var <- VarName
      index = 0
      for (i in 1:length(colnames(inflow))) {
        if (colnames(inflow)[i] == Var) {
          index = i
        }
      }
      inflowload <- c(rep(NA, length(inflow$time)))
      outflowload <- c(rep(NA, length(inflow$time)))
      retention <- c(rep(NA, length(inflow$time)))
      nutrientconccounter <- 1
      sum <- 0
      count <- 0
      currYear <- dates[1]
      front <- 1
      for (z in 1:length(inflow$time)) {
        inflowload[z] <- inflow[z, index] * 30.97 * inflow$FLOW[z] * 89.4 / 1000 # could be time 14
        while (!is.na(days[nutrientconccounter]) && !is.na(uniquedays[z]) && days[nutrientconccounter] == uniquedays[z]) {
          sum = sum + (temp55[nutrientconccounter] * 89.4) # in kg
          nutrientconccounter = nutrientconccounter + 1
          count = count + 1
        }
        outflowload[z] <- (outflow[z, 2] + overflow$FLOW[z]) * (sum/count)
        count = 0
        sum = 0
        retention[z] <- (inflowload[z] - outflowload[z]) / inflowload[z]
        if (!is.na(dates[nutrientconccounter]) && (nutrientconccounter == length(dates) || dates[nutrientconccounter] != currYear)) {
          currYear <- dates[nutrientconccounter-1]  
          dataByDateFRP[currYear - 2009, 4] = assign(paste0('inflow_load_FRP', datesNew[currYear - 2009]), sum(inflowload[front:z]))
          dataByDateFRP[currYear - 2009, 5] = assign(paste0('outflow_load_FRP', datesNew[currYear - 2009]), sum(outflowload[front:z]))
          dataByDateFRP[currYear - 2009, 6] = assign(paste0('retention_FRP', datesNew[currYear - 2009]), (sum(inflowload[front:z]) - sum(outflowload[front:z])) / (sum(inflowload[front:z])))
          front = z
        }
      }
      totalFRP <- data.frame(cbind(data.frame(uniquedays[1:1553]), inflowload, outflowload, retention))
      mean_mean_FRP<-mean(na.exclude(temp59)) #mean FRP of entire water column
      max_max_FRP<-max(na.exclude(temp56)) #max FRP of entire water column
      max_surface_FRP<-max(na.exclude(temp61)) #max FRP of surface waters
      mean_surface_FRP<-mean(na.exclude(temp55)) #mean FRP of surface waters
      max_sed_FRP<-max(na.exclude(temp62)) #max FRP of sediment 0-2 m 
      mean_sed_FRP<-mean(na.exclude(temp60)) #mean FRP of sediment 0-2 m
      total_inload_FRP <- sum(inflowload)
      total_outload_FRP <- sum(outflowload)
      mean_inload_FRP <- mean(inflowload)
      mean_outload_FRP <- mean(outflowload)
      colnames(dataByDateFRP) <- c('mean_mean_FRP', 'max_max_FRP', 'date_max_max_FRP', 'inflow_load_FRP', 'outflow_load_FRP', 'retention_FRP')
      rownames(dataByDateFRP) <- datesNew 
    }
    if(VarName=="NIT_denit"){
      Bottom <- findBottom()
      Surface <- findSurface(maxDepth)
      Sed <- findBottomSed()
      currYear <- dates[1]
      maxData <- resetVars(c(0), 1, c(0))
      k <- 1
      col <- 1
      front <- 1
      dataByDateDenit <- matrix(data = NA, nrow = length(datesNew), ncol = 3)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp51[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp52[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp76[j] <- (sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4]))))
        temp77[j] <- (max(na.exclude(Surface[j,2:4])))
        temp78[j] <- (sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4]))))
        temp79[j] <- (max(na.exclude(Sed[j,2:3]))) 
        maxData <- findMax(maxData, temp51, j, temp52)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateDenit[col, 1] <- assign(paste0('mean_mean_denit', datesNew[col]), mean(na.exclude((temp51[front:k]))))
          dataByDateDenit[col, 2] <- assign(paste0('max_max_denit', datesNew[col]), max(na.exclude(temp52[front:k])))
          dataByDateDenit[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp51, j, temp52)
        }
        k = k + 1
      }
      mean_mean_denit<-mean(na.exclude(temp51)) #mean denitrification rate of entire water column 
      max_max_denit<-max(na.exclude(temp52)) #max denitrification rate of entire water column 
      mean_surface_denit <- mean(na.exclude(temp76))
      max_surface_denit <- max(na.exclude(temp77))
      mean_sed_denit <- mean(na.exclude(temp78))
      max_sed_denit <- max(na.exclude(temp79))
      colnames(dataByDateDenit) <- c('mean_mean_denit', 'max_max_denit', 'date_max_denit')
      rownames(dataByDateDenit) <- datesNew 
    }
    if(VarName=="NIT_nitrif"){
      Bottom <- findBottom()
      Surface <- findSurface(maxDepth)
      Sed <- findBottomSed()
      currYear <- dates[1]
      maxData <- resetVars(c(0), 1, c(0))
      k <- 1
      col <- 1
      front <- 1
      dataByDateNitrif <- matrix(data = NA, nrow = length(datesNew), ncol = 3)
      for(j in 1:length(myOriginalDataPC$DateTime)){
        temp56[j]<-(sum(na.exclude(Bottom[j,2:35])))/(34-length(which(is.na(Bottom[j,2:35])))) #pull out only the 0-33 m intervals for each timestep and take avg
        temp57[j]<-(max(na.exclude(Bottom[j,2:35])))
        temp80[j] <- (sum(na.exclude(Surface[j,2:4])))/(3-length(which(is.na(Surface[j,2:4]))))
        temp73[j] <- (max(na.exclude(Surface[j,2:4])))
        temp74[j] <- (sum(na.exclude(Sed[j,2:4])))/(3-length(which(is.na(Sed[j,2:4]))))
        temp75[j] <- (max(na.exclude(Sed[j,2:3]))) 
        maxData <- findMax(maxData, temp56, j, temp57)
        if (dates[j] != currYear || j == length(dates)) {
          currYear <- dates[j-1] 
          dataByDateNitrif[col, 1] <- assign(paste0('mean_mean_nitrif', datesNew[col]), mean(na.exclude((temp56[front:k]))))
          dataByDateNitrif[col, 2] <- assign(paste0('max_max_nitrif', datesNew[col]), max(na.exclude(temp57[front:k])))
          dataByDateNitrif[col, 3] <- maxData[3]
          col = col + 1
          front = k + 1
          currYear <- dates[j]
          maxData <- resetVars(temp56, j, temp57)
        }
        k = k + 1
      }
      mean_mean_nitrif<-mean(na.exclude(temp56)) #mean nitrifrification rate of entire water column 
      max_max_nitrif<-max(na.exclude(temp57)) #max nitrifrification rate of entire water column 
      mean_surface_nitrif <- mean(na.exclude(temp80))
      max_surface_nitrif <- max(na.exclude(temp73))
      mean_sed_nitrif <- mean(na.exclude(temp74))
      max_sed_nitrif <- max(na.exclude(temp75))
      colnames(dataByDateNitrif) <- c('mean_mean_nitrif', 'max_max_nitrif', 'date_max_nitrif')
      rownames(dataByDateNitrif) <- datesNew 
    }
  }
  myOriginalDataPCAll = get_var(SimFile,VarName) 
  depthsUsed<-get.offsets(myOriginalDataPCAll)
  
  depths <- c(0,5,10,15,20,25,30,33)
  frp_all <- get_var(SimFile, 'PHS_frp', z_out = depths)
  nit_all <- get_var(SimFile, 'NIT_nit', z_out = depths)
  amm_all <- get_var(SimFile, 'NIT_amm', z_out = depths)
  oxy_all <- get_var(SimFile, 'OXY_oxy', z_out = depths)
  zoo1 <- get_var(SimFile, 'ZOO_COPEPODS1', z_out = depths)
  zoo2 <- get_var(SimFile, 'ZOO_DAPHNIABIG2', z_out = depths)
  zoo3 <- get_var(SimFile, 'ZOO_DAPHNIASMALL3', z_out = depths)
  #file.remove(list.files('.'))
} else{ Message = paste('Experiment unsuccessful', sep="") 
print(Message)
} #end of for loop of # of sims if there is more than one folder


dataset<-data.frame(mean_mean_temp,max_max_temp,mean_surface_temp,max_surface_temp,mean_bottom_temp,max_bottom_temp,min_bottom_temp,mean_0_2_temp,max_0_2_temp,mean_bottom_oxy,min_bottom_oxy,hypoxic_length,anoxic_length,mean_mean_oxy2,max_max_oxy2,mean_0_2_oxy2,max_0_2_oxy2,mean_sed_oxy2,max_sed_oxy2,mean_mean_TN,max_max_TN,mean_0_2_TN,max_0_2_TN,mean_sed_TN,mean_sed_TP,mean_mean_TP,max_max_TP,mean_0_2_TP,
                    max_0_2_TP,mean_sed_TP, max_sed_TP, mean_mean_totalphytos,max_max_totalphytos,mean_0_2_totalphytos,max_0_2_totalphytos,mean_mean_totalchla,max_max_totalchla,mean_0_2_totalchla,max_0_2_totalchla,mean_mean_Nfixingcyanos,max_max_Nfixingcyanos,mean_0_2_Nfixingcyanos,max_0_2_Nfixingcyanos,mean_mean_nonNfixingcyanos,
                    max_max_nonNfixingcyanos,mean_0_2_nonNfixingcyanos,max_0_2_nonNfixingcyanos,mean_mean_chlorophytes,max_max_chlorophytes,mean_0_2_chlorophytes,max_0_2_chlorophytes,mean_mean_diatoms,max_max_diatoms,mean_0_2_diatoms,max_0_2_diatoms,mean_mean_GPP,max_max_GPP,mean_0_2_GPP,max_0_2_GPP,
                    mean_mean_TSS,max_max_TSS, mean_mean_DINsedflux, mean_mean_DIPsedflux, mean_mean_NIT, max_max_NIT, mean_surface_NIT, max_surface_NIT, mean_sed_NIT, max_sed_NIT, total_inload_NIT, total_outload_NIT, mean_inload_NIT, mean_outload_NIT, mean_mean_AMM, max_max_AMM, mean_surface_AMM, max_surface_AMM, mean_sed_AMM, max_sed_AMM, total_inload_AMM, total_outload_AMM, mean_inload_AMM, mean_outload_AMM, 
                    mean_mean_FRP, max_max_FRP, mean_surface_FRP, max_surface_FRP, mean_sed_FRP, max_sed_FRP, total_inload_FRP, total_outload_FRP, mean_inload_FRP, mean_outload_FRP, mean_mean_denit, max_max_denit, mean_surface_denit, max_surface_denit, mean_sed_denit, max_sed_denit, mean_mean_nitrif, max_max_nitrif, mean_surface_nitrif, 
                    max_surface_nitrif, mean_sed_nitrif, max_sed_nitrif )
dataSetByTime <- data.frame(datesNew, data.frame(dataByDateTemp), dataByDateOxy, dataByDateOxy2, dataByDateTOTTn, dataByDateTOTTp, dataByDateTPHYS, dataByDateTCHLA, dataByDatePCH1, dataByDatePCH2, dataByDatePCH3, dataByDatePCH4, dataByDatePPR, dataByDateTSS, dataByDateIce, dataByDateDINsedflux, dataByDateDIPsedflux,
                            dataByDateNIT, dataByDateAMM, dataByDateFRP, dataByDateDenit, dataByDateNitrif)   

write.csv(dataset, "Sim__.csv", row.names = FALSE)
write.csv(overflow, 'overflow.csv', row.names = FALSE)
write.csv(glm_surface, "Lake_Depth_Flux_Simu_.csv", row.names = FALSE)
write.csv(dataSetByTime, "Data_By_Time_GRAPLEready.csv", row.names = FALSE)

oxy_vars <- cbind(myOriginalDataPC$DateTime, temp41, temp73, temp42, temp72)
colnames(oxy_vars) <- c("time", "DIN_sed_nit", "DIN_sed_amm", "PHS_sed_frp", "OXY_sed_oxy")
write.csv(oxy_vars, "sed_rates.csv", row.names = FALSE, quote = FALSE)
write.csv(cbind(frp_all, nit_all, amm_all, oxy_all), 'timeseries_nutrient.csv', row.names = FALSE, quote = FALSE)
# DIN_sed_nit, DIN_sed_amm, PHS_sed_frp, OXY_sed_oxy

write.csv(cbind(zoo1, zoo2, zoo3), 'zooplankton_out.csv', row.names = FALSE, quote = FALSE)

#totalNIT, totalAMM, totalFRP
loads <- cbind(totalNIT, totalAMM, totalFRP)
write.csv(loads, "sim_loads.csv", row.names = FALSE, quote = FALSE)

