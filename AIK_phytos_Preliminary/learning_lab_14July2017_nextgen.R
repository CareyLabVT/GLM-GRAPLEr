# Goes through an interesting output of a "maxima_phytos" script & converts to numeric
# Some statistical transformations possible
# Written by Arianna Krinos, last edits on 14 July 2017

setwd("C:/Users/arian_000/Documents/Summer 2017/Carey Lab/108k Sim Results/")
#current = read.csv("3var_play.csv") # Use this or a created current obj from partner script
numbers = c(0) # 1 if a max, 0 if a day
codes = c(0) # a system of codes describing conditions
# A code of 2 means Day, Below
# A code of 3 means Day, Above
# A code of 4 means Max, Below
# A code of 5 means Max, Above
heightnumbers = c(0) # 1 if above, 0 if below
for (l in 2:length(current$descriptors)) { 
  codeSave = FALSE
  if (length((grep("Max", current$descriptors[l]))) != 0) {
    numbers = rbind(numbers, 1) 
    codeSave = TRUE
  }
  else {
    numbers = rbind(numbers, 0) 
  }
  if (length((grep("Above", current$descriptors[l]))) != 0) {
    heightnumbers = rbind(heightnumbers, 1) 
    if (codeSave) {
      codes = rbind(codes, 5)
    }
    else {
      codes = rbind(codes, 3)
    }
  }
  else {
    heightnumbers = rbind(heightnumbers, 0) 
    if (codeSave) {
      codes = rbind(codes, 4)
    }
    else {
      codes = rbind(codes, 2)
    }
  }
}

newframe = cbind(current, numbers, heightnumbers, codes) 
colnames(newframe) = cbind(t(colnames(current)), "MaxOrDay", "AboveOrBelow", "Combo Code")

plot(newframe$AirTemp, main = "Air temperatures", type = "p")
points(newframe$MaxOrDay, col = "red")
points(newframe$AboveOrBelow, col = "blue")


write.csv(newframe, "transformeddata_codes2.csv", row.names = FALSE)

if (length(na.exclude(as.numeric(unique(newframe$WindSpeed)))) > 1) {
  windanalysis = matrix(0, ncol = length(na.exclude(as.numeric(unique(newframe$WindSpeed)))), nrow = length(unique(newframe$`Combo Code`))) 
  colnames(windanalysis) = as.numeric(sort(na.exclude(as.numeric(unique(newframe$WindSpeed))), decreasing = FALSE))
  rownames(windanalysis) = as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))
  
  for (p in 1:length(newframe$WindSpeed)) {
    if (p > 2 && !is.na(as.numeric(newframe$WindSpeed[p])) && as.numeric(newframe$WindSpeed[p]) == 0 && as.numeric(newframe$WindSpeed[p - 2]) == 0.1) {
      break
    }
    for (g in 1:length(windanalysis[1,])) {
      if (colnames(windanalysis)[g] == newframe$WindSpeed[p]) {
        break
      }
    }
    for (q in 1:length(windanalysis[,1])) {
      if (rownames(windanalysis)[q] == newframe$`Combo Code`[p]) {
        break
      }
    }
    windanalysis[q,g] = windanalysis[q,g] + 1
  }
  legendlabs = c(rep("no", length(rownames(windanalysis)) - 1))
  plot(windanalysis[2,], col = "red", main = "WindSpeed Impacts", xlab = "WindSpeed Increment", ylab = "Number of trips", ylim = c(min(windanalysis), max(windanalysis)))#, xaxt = "n")
  #axis(1,as.numeric(colnames(windanalysis)) + 1)
  cols = c("red", "blue", "pink", "purple")
  for (e in 1:(length(rownames(windanalysis)) - 1)) {
    points(windanalysis[(e + 1),], col = colsorig[e])
    lines(windanalysis[(e + 1),], col = colsorig[e])
    if (rownames(windanalysis)[e + 1] == 2) {
      legendlabs[e] = "Day of max, below"
      cols[e] = colsorig[e]
    }
    else if (rownames(windanalysis)[e + 1] == 3) {
      legendlabs[e] = "Day of max, above"
      cols[e] = colsorig[e]
    }
    else if (rownames(windanalysis)[e + 1] == 4) {
      legendlabs[e] = "Max, below"
      cols[e] = colsorig[e]
    }
    else {
      legendlabs[e] = "Max, above"
      cols[e] = colsorig[e]
    }
  }
  legend("topleft", legendlabs, fill = cols)
  
  write.csv(windanalysis, paste0("WindSpeedNumbers", runningTotal, ".csv"))
  
  write.csv(windanalysis, paste0("WindNumbers", runningTotal, ".csv"))
}

if (length(na.exclude(as.numeric(unique(newframe$RelHum)))) > 1) {
  humanalysis = matrix(0, nrow = length(as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))), ncol = length(as.numeric(sort(unique(newframe$RelHum), decreasing = FALSE)))) 
  colnames(humanalysis) = as.numeric(sort(unique(newframe$RelHum), decreasing = FALSE))
  rownames(humanalysis) = as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))
  
  for (p in 1:length(newframe$RelHum)) {
    if (p > 2 && !is.na(as.numeric(newframe$RelHum[p])) && as.numeric(newframe$RelHum[p]) == 0 && as.numeric(newframe$RelHum[p - 2]) == 0.1) {
      break
    }
    for (g in 1:length(humanalysis[1,])) {
      if (colnames(humanalysis)[g] == newframe$RelHum[p]) {
        break
      }
    }
    for (q in 1:length(humanalysis[,1])) {
      if (rownames(humanalysis)[q] == newframe$`Combo Code`[p]) {
        break
      }
    }
    humanalysis[q,g] = humanalysis[q,g] + 1
  }
  
  plot(humanalysis[2,], col = "red", main = "Rel Hum Impacts", xlab = "Rel Hum Increment", ylab = "Number of trips", xaxt = "n", ylim = c(min(humanalysis), max(humanalysis)))
  colsorig = c("red", "blue", "pink", "purple")
  legendlabs = c(rep("no", length(rownames(humanalysis)) - 1))
  axis(1,as.numeric(as.character(colnames(humanalysis))) + 1)
  for (e in 1:(length(rownames(humanalysis)) - 1)) {
    points(humanalysis[(e + 1),], col = colsorig[e])
    lines(humanalysis[(e + 1),], col = colsorig[e])
    if (rownames(humanalysis)[e + 1] == 2) {
      legendlabs[e] = "Day of max, below"
      cols[e] = colsorig[e]
    }
    else if (rownames(humanalysis)[e + 1] == 3) {
      legendlabs[e] = "Day of max, above"
      cols[e] = colsorig[e]
    }
    else if (rownames(humanalysis)[e + 1] == 4) {
      legendlabs[e] = "Max, below"
      cols[e] = colsorig[e]
    }
    else {
      legendlabs[e] = "Max, above"
      cols[e] = colsorig[e]
    }
  }
  legend("topleft", legendlabs, fill = cols)
  
  write.csv(humanalysis, paste0("humNumbers", runningTotal, ".csv"))
}

if (length(na.exclude(as.numeric(unique(newframe$AirTemp)))) > 1) {
  AirTempanalysis = matrix(0, nrow = length(as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))), ncol = length(na.exclude(as.numeric(unique(newframe$AirTemp))))) 
  colnames(AirTempanalysis) = as.numeric(sort(na.exclude(as.numeric(unique(newframe$AirTemp))), decreasing = FALSE))
  rownames(AirTempanalysis) = as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))
  
  keepGoing = TRUE
  for (p in 1:length(newframe$AirTemp)) {
    if (p > 2 && !is.na(as.numeric(newframe$AirTemp[p])) && as.numeric(newframe$AirTemp[p]) == 0 && as.numeric(newframe$AirTemp[p - 2]) == 7) {
      break
    }
    for (g in 1:length(AirTempanalysis[1,])) {
      if (colnames(AirTempanalysis)[g] == newframe$AirTemp[p]) {
        break
      }
    }
    for (q in 1:length(AirTempanalysis[,1])) {
      if (rownames(AirTempanalysis)[q] == newframe$`Combo Code`[p]) {
        break
      }
    }
    AirTempanalysis[q,g] = AirTempanalysis[q,g] + 1
  }
  
  plot(AirTempanalysis[2,], col = "red", main = "Air Temperature Impacts", xlab = "Air Temp Increment", ylab = "Number of trips", xaxt = "n", ylim = c(min(AirTempanalysis), max(AirTempanalysis)))
  colsorig = c("red", "blue", "pink", "purple")
  legendlabs = c(rep("no", length(rownames(AirTempanalysis)) - 1))
  axis(1,as.numeric(as.character(colnames(AirTempanalysis))) + 1)
  for (e in 1:(length(rownames(AirTempanalysis)) - 1)) {
    points(AirTempanalysis[(e + 1),], col = colsorig[e])
    lines(AirTempanalysis[(e + 1),], col = colsorig[e])
    if (rownames(AirTempanalysis)[e + 1] == 2) {
      legendlabs[e] = "Day of max, below"
      cols[e] = colsorig[e]
    }
    else if (rownames(AirTempanalysis)[e + 1] == 3) {
      legendlabs[e] = "Day of max, above"
      cols[e] = colsorig[e]
    }
    else if (rownames(AirTempanalysis)[e + 1] == 4) {
      legendlabs[e] = "Max, below"
      cols[e] = colsorig[e]
    }
    else {
      legendlabs[e] = "Max, above"
      cols[e] = colsorig[e]
    }
  }
  legend("topleft", legendlabs, fill = cols)
  
  write.csv(AirTempanalysis, paste0("AirTempNumbers", runningTotal, ".csv"))
}

if (length(na.exclude(as.numeric(unique(newframe$flow)))) > 2) {
  flowanalysis = matrix(0, nrow = length(as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))), ncol = length(as.numeric(sort(unique(newframe$flow), decreasing = FALSE)))) 
  colnames(flowanalysis) = as.numeric(sort(unique(newframe$flow), decreasing = FALSE))
  rownames(flowanalysis) = as.numeric(sort(unique(newframe$`Combo Code`), decreasing = FALSE))
  
  for (p in 1:length(newframe$flow)) {
    if (p > 2 && !is.na(as.numeric(newframe$flow[p])) && as.numeric(newframe$flow[p]) == 0 && as.numeric(newframe$flow[p - 2]) == 1.4) {
      break
    }
    for (g in 1:length(flowanalysis[1,])) {
      if (colnames(flowanalysis)[g] == newframe$flow[p]) {
        break
      }
    }
    for (q in 1:length(flowanalysis[,1])) {
      if (rownames(flowanalysis)[q] == newframe$`Combo Code`[p]) {
        break
      }
    }
    flowanalysis[q,g] = flowanalysis[q,g] + 1
  }
  plot(flowanalysis[2,], col = "red", main = "flow Impacts", xlab = "flow Increment", ylab = "Number of trips", ylim = c(min(flowanalysis), max(flowanalysis)))#, xaxt = "n")
  #axis(1,as.numeric(colnames(flowanalysis)) + 1)
  cols = c("red", "blue", "pink", "purple")
  for (e in 1:(length(rownames(flowanalysis)) - 1)) {
    points(flowanalysis[(e + 1),], col = colsorig[e])
    lines(flowanalysis[(e + 1),], col = colsorig[e])
    if (rownames(flowanalysis)[e + 1] == 2) {
      legendlabs[e] = "Day of max, below"
      cols[e] = colsorig[e]
    }
    else if (rownames(flowanalysis)[e + 1] == 3) {
      legendlabs[e] = "Day of max, above"
      cols[e] = colsorig[e]
    }
    else if (rownames(flowanalysis)[e + 1] == 4) {
      legendlabs[e] = "Max, below"
      cols[e] = colsorig[e]
    }
    else {
      legendlabs[e] = "Max, above"
      cols[e] = colsorig[e]
    }
  }
  legend("topleft", legendlabs, fill = cols)
  
  
  write.csv(flowanalysis, paste0("flowNumbers", runningTotal, ".csv"))
  
  
}

runningTotal = runningTotal  + 1
