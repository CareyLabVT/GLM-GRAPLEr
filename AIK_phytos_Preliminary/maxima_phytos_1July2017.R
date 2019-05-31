# Grab the most extreme results
# Written by Arianna Krinos, last edits on 27 June 2017

setwd("C:/Users/arian_000/Documents/Summer 2017/Carey Lab/108k Sim Results/")
#setwd("C:/Users/arian_000/Documents/Summer 2017/Carey Lab/MyResults_MendotaPhytos_27June/AIK_Mendota_27June/")

curr_directory = getwd()
sim_summary <- read.csv("sim_summary.csv", header = FALSE)
colnames(sim_summary) <- lapply(sim_summary[1,], as.character)

temps = c(0:7) # selected if you want to find temps 
rain = c(0, 0.02, 0.04, 0.06, 0.08, 0.1) 
flow = c(0.8, 0.92, 1.04, 1.16, 1.28, 1.4) 

flownum = which(colnames(sim_summary) == "flow") # Get the column number of col labeled "flow"
flow = unique(sim_summary[,(flownum) + 3]) # the same can be done for any of them

windnum = which(colnames(sim_summary) == "WindSpeed") # Get the column number of col labeled "Wind Speed'
WindSpeed = unique(sim_summary[,(windnum) + 3]) # the same can be done for any of them


colname_folders <- c(rep(NA, ((length(colnames(sim_summary)) - 1) / 4) + 1))
conditionsavail <- c(rep(NA, ((length(colnames(sim_summary)) - 1) / 4)))
colname_folders[1] <- "Sim Folder"
for (j in 0:((length(colnames(sim_summary)) - 1)/4 - 1)) { 
  colname_folders[j+2] <- colnames(sim_summary)[2 + j * 4]
  assign(paste0(colname_folders[j+2], "_conditions"), sim_summary[5 + j * 4])
  conditionsavail[j + 1] <- paste0(colname_folders[j+2])
}
varos = conditionsavail

print(paste("Available variable names include:", varos[1], varos[2], varos[3], varos[4], varos[5], varos[6], varos[7], sep = " "))
print(paste("It is easier if you enter them in the order they are written. "))
varsnolikepract <- readline("Enter togglevars separated by only commas (no spaces): ");
varsnolike <- as.character(unlist(strsplit(varsnolikepract, ",")));

for (k in 1:length(varsnolike)) {
  assign(paste0("var", k), varsnolike[k])
}

for (m in 1:length(varos)) {
  found = FALSE
  for (o in 1:length(varsnolike)) {
    if (varsnolike[o] == varos[m]) {
      found = TRUE
    }
  }
  if (!found) {
    k = k + 1
    assign(paste0("var", k), varos[m])
  }
}

for (j in 1:length(varos)) {
  assign(paste0(eval(as.name(paste0("var", j))), "num"), which(colnames(sim_summary) == eval(as.name(paste0("var", j)))))
  assign(eval(as.name(paste0("var", j))), unique(sim_summary[,eval(as.name(paste0(eval(as.name(paste0("var", j))), "num"))) + 3]))
}

flowbasel = 1.04 # What counts as a baseline number for each of the parameters? 
flowbasel = as.numeric(readline("What is your baseline number for flow (probably 1 or 1.04)? "))
PHS_frpbasel = 1.0
NIT_ammbasel = 1.0
Rainbasel = 0
RelHumbasel = 0
AirTempbasel = 0
WindSpeedbasel = 0
firstOne = TRUE
count = 2

conditions = c(rep(NA, length(varos)))

for (i in 1:length(sim_summary[,1])) {
  goodguy = TRUE 
  if (length(varsnolike) != length(varos)) {
  for (l in (length(varsnolike) + 1):length(varos)) {
    if (sim_summary[i,(eval(as.name(paste0(eval(as.name(paste0("var", l))), "num"))) + 3)] != eval(as.name(paste0(eval(as.name(paste0("var", l))), "basel")))) {
      goodguy = FALSE
    }
  }
  }
  if (goodguy) {
    if (firstOne) {
      foldernames = c(as.character(sim_summary$Sim1_1[i]))
      conditions = rbind(conditions, c(rep(NA, length(varos))))
      for (g in 1:length(varos)) {
        conditions[count, g] = sim_summary[i, 5 + 4 * (g -1)]
      }
      count = count + 1
      firstOne = FALSE
    }
    else { 
      foldernames = cbind(foldernames, c(as.character(sim_summary$Sim1_1[i])))
      conditions = rbind(conditions, c(rep(NA, length(varos))))
      for (g in 1:length(varos)) {
        conditions[count, g] = sim_summary[i, 5 + 4 * (g -1)]
      }
      count = count + 1
    }
  }
}

colnames(conditions) = varos

example = read.csv(paste0(curr_directory, "/Sims/", foldernames[j - 1], "/Results/Sim__.csv"))

firstOne = TRUE
for (j in 2:(length(foldernames) + 1 )) {
  firstT = TRUE
  for (l in 1:length(unique(example$datesNew))) {
    if (firstT) { 
      copiesconds = conditions[j,]
      firstT = FALSE
    }
    else { 
      copiesconds = rbind(copiesconds, conditions[j,])
    }
  }
  if (firstOne) {
    yearly <- cbind(conditions[j,], read.csv(paste0(curr_directory, "/Sims/", foldernames[j - 1], "/Results/Sim__.csv")))
    annual <- cbind(copiesconds, read.csv(paste0(curr_directory, "/Sims/", foldernames[j - 1], "/Results/Data_By_Time.csv")))
    firstOne = FALSE
  }
  else {
    yearly <- rbind(yearly, cbind(conditions[j,], read.csv(paste0(curr_directory, "/Sims/", foldernames[j - 1], "/Results/Sim__.csv"))))
    annual <- rbind(annual, cbind(copiesconds, read.csv(paste0(curr_directory, "/Sims/", foldernames[j - 1], "/Results/Data_By_Time.csv"))))
  }
}


# We want to get the baseline mean and standard deviation for diatom max and day of max. 
# We need to iterate through totaltotal to grab the 0 temp iterations and then average
# that out, 2011-2015; take std dev. 

for (z in 1:length(colnames(annual))) {
  if (length(grep("date", colnames(annual)[z])) != 0 && colnames(annual)[z] != "datesNew") {
    for (j in 1:length(annual[,z])) {
      annual[j,z] = (as.POSIXlt(as.numeric(annual[j,z]), origin = "1970-01-01"))$yday
    }
  }
}

meanbaselinediatom = 0
meanbaselinediatomday = 0
stddevframe = c(rep(0, 5))
stddevframe2 = c(rep(0, 5))
days = 0
bookmark = 0
columnnums = c(0)
for (y in 1:length(colnames(annual))) {
  for (q in 1:length(varsnolike)) {
    if (colnames(annual)[y] == eval(as.name((paste0("var", q))))) {
      assign(paste0("columnnum", q), y)
      columnnums = cbind(columnnums, y)
    }
  }
}

for (i in 1:length(annual[,1])) { 
  evaluate = TRUE
  if (annual$datesNew[i] != 2010) { 
    for (g in 1:length(varsnolike)) {
      if (annual[i, columnnums[g + 1]] != eval(as.name(paste0((as.name(varsnolike[g])), "basel")))) {
        evaluate = FALSE
      }
    }
    if (evaluate) { 
      meanbaselinediatom = meanbaselinediatom + as.numeric(as.character(annual$max_0_2_diatoms[i]))
      meanbaselinediatomday = meanbaselinediatomday + as.numeric(as.character(annual$date_max_0_2_diatoms[i]))
      assign(paste0("meanbaselinediatom", annual$datesNew[i]), as.numeric(as.character(annual$max_0_2_diatoms[i])))
      assign(paste0("meanbaselinediatomday", annual$datesNew[i]), as.numeric(as.character(annual$date_max_0_2_diatoms[i])))
      days = days + 1
      stddevframe[days] = as.numeric(as.character(annual$max_0_2_diatoms[i]))
      stddevframe2[days] = as.numeric(as.character(annual$date_max_0_2_diatoms[i]))
    }
  }
}


meanbaselinediatom = meanbaselinediatom / days
meanbaselinediatomday = meanbaselinediatomday / days 
stdevmean = sd(stddevframe)
stdevdays = sd(stddevframe2)
numbersneeded = 0

for (j in 2:length(columnnums)) {
  assign(paste0("ticks", j), c(unique(annual[,columnnums[j]])))
  numbersneeded = numbersneeded + length(c(unique(annual[,columnnums[j]])))
}

threshold = stdevmean * 3 #should be x3
thresholddays = stdevdays * 3
dataframe_overall = matrix(0, nrow = 2, ncol = numbersneeded)
bookmarknum = length(varsnolike)
totalcounter = 1
listcrossedthreshold = c(rep(0, 8))
descriptors = 0

for (i in 1:length(annual[,1])) {
  bookmarkcounter = 1
  backsplash = 0
  found = FALSE
  if (annual$datesNew[i] != 2010) { 
    for (j in 2:length(columnnums)) {
      for (k in 1:length(eval(as.name(paste0("ticks",j))))) {
        if (as.numeric(as.character(annual[i,columnnums[j]])) == eval(as.name(paste0("ticks", j)))[k]) {
          assign(paste0("bookmark", j), k + backsplash)
          totalcounter = totalcounter + 1
          bookmarkcounter = bookmarkcounter + 1
          found = TRUE
        }
      }
      if (found) {
        backsplash = backsplash + length(eval(as.name(paste0("ticks",j)))) # Add the extra numbers you've already passed to move up in vector
      }
    }
    if (((as.numeric(as.character(annual$max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatom", annual$datesNew[i])))) > threshold) || ((as.numeric(as.character(annual$max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatom", annual$datesNew[i])))) < (-threshold)) ) {
      for (l in 2:length(columnnums)) {
        dataframe_overall[1, eval(as.name(paste0("bookmark", l)))] = dataframe_overall[1, eval(as.name(paste0("bookmark", l)))] + 1
      }
      if ((as.numeric(as.character(annual$max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatom", annual$datesNew[i])))) > threshold) {
        descriptors = rbind(descriptors, "Biomass Max, Above")
      }
      else {
        descriptors = rbind(descriptors, "Biomass Max, Below")
      }
      listcrossedthreshold = rbind(listcrossedthreshold, cbind(annual[i,1:8], annual$date_max_0_2_diatoms[i], annual$max_0_2_diatoms[i]))
    }
    if (((as.numeric(as.character(annual$date_max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatomday", annual$datesNew[i])))) > thresholddays) || ((as.numeric(as.character(annual$date_max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatomday", annual$datesNew[i])))) < (-thresholddays)) ) {
      for (n in 2:length(columnnums)) {
        dataframe_overall[2, eval(as.name(paste0("bookmark", n)))] = dataframe_overall[2, eval(as.name(paste0("bookmark", n)))] + 1
      }
      if ((as.numeric(as.character(annual$max_0_2_diatoms[i])) - eval(as.name(paste0("meanbaselinediatomday", annual$datesNew[i])))) > thresholddays) {
        descriptors = rbind(descriptors, "Biomass Day, Above")
      }
      else {
        descriptors = rbind(descriptors, "Biomass Day, Below")
      }
      listcrossedthreshold = rbind(listcrossedthreshold, cbind(annual[i,1:8], annual$date_max_0_2_diatoms[i], annual$max_0_2_diatoms[i])) #eventually get rid of this
      
    }
  }
}

found = FALSE
for (k in 2:(length(varsnolike) + 1)) {
  if (!found) {
    ticksall = t(c(eval(as.name(paste0("ticks",k)))))
    found = TRUE
  }
  else {
    ticksall = cbind(ticksall, t(c(eval(as.name(paste0("ticks",k))))))
  }
}

colnames(dataframe_overall) = ticksall 
rownames(dataframe_overall) = c("Maximum Diatoms", "Date Max Diatoms")

first = TRUE
for (i in 1:length(varsnolike)) {
  if (first) {
    varnames = varsnolike[i]
    first = FALSE
  }
  else {
    varnames = paste(varnames, varsnolike[i], sep = "_")
  }
}
write.csv(dataframe_overall, paste0(varnames, "_elsewhere0_Counts_Threshold_Diatoms_3.csv"), row.names = TRUE)
write.csv(cbind(listcrossedthreshold, descriptors), paste0(varnames, "_list_dates_Threshold_Diatoms_3.csv"), row.names = TRUE)
