# Grab the simulation results quickly! ####
# Written by Arianna Krinos, last edits by KJF on 25 May 2017

#install.packages('gplots') #if you are without gplots 
#install.packages('plotrix') #if you are without plotrix
library(gplots) #if you just opened R
library(plotrix) #if you just opened R


# You need these numbers if you aren't iterating through *all* heatmaps
# You can go ahead and type in numbers regardless though, won't matter (otherwise comment out)
column <- as.numeric(readline(prompt = "Tell me what variable you're interested in (from namekey.csv): ")) + 3
column2 <- as.numeric(readline(prompt = "Tell me what variable you're interested in (from namekey2.csv): "))


#setwd("C:/Users/arian_000/Documents/Spring 2017/Carey Lab/MyResults_Sunapee10May/AIK_Sunapee_10May") # AIK working directory 
setwd("C:/Users/farrellk/Documents/GRAPLER N & P/AIK_Sunapee_10May") # KF working directory
#setwd("./AIK_Sunapee_10May") # If I'm able to upload 10May results folder to GitHub, this will run wd through GLM-GRAPLEr project

# GENERAL SECTION - must be run no matter what to do anything else with this script ####
# section presently ends on line 104
sim_summary <- read.csv("sim_summary.csv", header = FALSE)
colnames(sim_summary) <- lapply(sim_summary[1,], as.character)

folders_to_find <- matrix(NA, ncol = (length(colnames(sim_summary)) - 1 ) / 4 + 1, nrow = length(sim_summary[,1])) # Expand this if you're looking for multiple factors
folders_to_find2 <- matrix(NA, ncol = (length(colnames(sim_summary)) - 1 ) / 4 + 1, nrow = length(sim_summary[,1])) # Expand this if you're looking for multiple factors

colname_folders <- c(rep(NA, ((length(colnames(sim_summary)) - 1) / 4) + 1))
conditions <- c(rep(NA, ((length(colnames(sim_summary)) - 1) / 4)))
colname_folders[1] <- "Sim Folder"
for (j in 0:((length(colnames(sim_summary)) - 1)/4 - 1)) { 
  colname_folders[j+2] <- colnames(sim_summary)[2 + j * 4]
  assign(paste0(colname_folders[j+2], "_conditions"), sim_summary[5 + j * 4])
  conditions[j + 1] <- paste0(colname_folders[j+2], "_conditions")
}
colnames(folders_to_find) <- colname_folders
print("Below are the conditions present in the sim folder: ", quote = FALSE)
print(conditions)
print("Type the name of the first analysis variable: ", quote = FALSE)
#var1 <- readline(prompt = "enter it here: ")
#var2 <- readline(prompt = "second please: ")
var2 <- "AirTemp_conditions" #listing as variable means you don't need eval(as.name()) or the deparse below
var1 <- "FLOW_conditions" #"Rain_conditions"
# alternatively,  just set var1 and var2 (needed outside interactive mode)
exclude <- c(rep(NA, length(conditions) - 2))
tick <- 1
for (k in 1:length(conditions)) {
  if (as.name(conditions[k]) != as.name(var1) && as.name(conditions[k]) != as.name(var2)) {
    exclude[tick] <- conditions[k]
    tick = tick + 1
  }
}

exclude2 <- c(rep(NA, length(conditions) - 1))
tick <- 1
for (k in 1:length(conditions)) {
  if (as.name(conditions[k]) != as.name(var1)) {
    exclude2[tick] <- conditions[k]
    tick = tick + 1
  }
}

folders <- data.frame(lapply(sim_summary[,1], as.character), stringsAsFactors = FALSE)
counter <- 1

for (i in 1:length(sim_summary[,1])) {
  save = TRUE
  if (length(exclude) != 0) {
  for (y in 1:length(exclude)) {
    if (eval(as.name(exclude[y]))[i,1] != 0 && eval(as.name(exclude[y]))[i,1] != 1) {
      save = FALSE
    }
  }
  }
  if (save) {
    folders_to_find[counter, 1] = folders[1, i]
    for (z in 2:(length(folders_to_find[1,]))) {
      folders_to_find[counter, z] = eval(as.name(conditions[z - 1]))[i,1]
    }
    counter = counter + 1
  }
}
folders_to_find <- folders_to_find[1:counter-1,] # Get rid of everything you don't need. 

counter <- 1

for (i in 1:length(sim_summary[,1])) {
  save = FALSE
  basel = TRUE
  if (length(exclude2) != 0) {
    for (y in 1:length(exclude2)) {
      if (eval(as.name(exclude2[y]))[i,1] == 0 || (eval(as.name(exclude2[y]))[i,1] == 1.0 && exclude2[y] == "FLOW_conditions")) {
        save = TRUE
      }
    }
  }
  if (save) {
    folders_to_find2[counter, 1] = folders[1, i]
    for (z in 2:(length(folders_to_find2[1,]))) {
      folders_to_find2[counter, z] = eval(as.name(conditions[z - 1]))[i,1]
      if (((folders_to_find2[counter, z] != 0 && var1 != "FLOW_conditions" && z == 3) || (folders_to_find2[counter,z] != 1 && var1 == "FLOW_conditions" && z == 2))){ #&& folders_to_find2[counter,z] != 1) {
        basel = FALSE
      }
    }
    
    if (basel) {
      baseline = folders[1, i]
    }
    counter = counter + 1
  }
}
folders_to_find2 <- folders_to_find2[1:counter-1,] # Get rid of everything you don't need.
 
curr_directory <- getwd()

sim_folders <- list.files(paste0(curr_directory, '/Sims'))

# INITIALIZATION OF NEEDED STORAGE UNITS ####
new_counter <- 1
new_counter_2 <-1
example <- read.csv(paste0(curr_directory, "/Sims/", sim_folders[1], "/Results/Sim__.csv"))
example2 <- read.csv(paste0(curr_directory, "/Sims/", sim_folders[1], "/Results/Data_By_Time_GRAPLEready.csv"))
example3 <- read.csv(paste0(curr_directory, "/Sims/", folders_to_find2[2,1], "/Results/timeseries_nutrient.csv"))
aggregate <- t(c(rep(0, length(example[1,]))))
aggregate2 <- t(c(rep(0, (length(example2[1,]) + length(folders_to_find2[1,]) - 1))))
aggregate3 <- c(rep(0, length(example3[,1]))) 
colnames(aggregate) <- colnames(example)
colnames(aggregate2) <- c((conditions), colnames(example2))
allconditions <- matrix(nrow = length(folders_to_find[,1]) + 1, ncol = length(conditions))

# PLOT TIME SERIES DATA FOR A GIVEN NUTRIENT ####
counter3 <- 1
legendlabels = c(rep(NA, length(folders_to_find2[,1])))
nutrientVar <- "PHS_frp" # change this for nutrient; either PHS_frp, NIT_amm, OXY_oxy, or NIT_nit
# NOTE: NIT_nit is still not working!! it'll look ridiculous
if (nutrientVar == "PHS_frp") {
  gapVect = c(0,0.5,1,1.5,2,2.5,3.2)
  gapVar = c(2.5,3)
} else if (nutrientVar == "NIT_amm") { 
  gapVect = c(0,0.5,1,1.5,2)
  gapVar = c(2,2.5)
} else { # Doesn't really work because of weird denominators
  gapVec = c(500,1000,3000)
  gapVar = c(1000,2000)
}
depth <- 30 # choose a depth, currently 0,5,10,15,20,25,30,33 for Sunapee; 0 = surface, 33 = sediments
cols = c("purple", "red", "green", "blue", "pink", "orange", "turquoise", "dark green")
ltys = c(1,1,1,1,1)
par(mfrow = c(1,1))
par(xpd = NA)
par(mar = c(4,4,4,4))
#par(oma=c(0,0,0,10))
baseline_nutrient <- read.csv(paste0(curr_directory, "/Sims/", baseline, "/Results/timeseries_nutrient.csv"))
totalName <- as.name(paste0(nutrientVar, ".elv_", depth)) 
for (i in 1:length(colnames(example3))) {
  if (colnames(example3)[i] == totalName) {
    colnum = i
  }
}
nutr_values = (example3[,colnum] / (baseline_nutrient[,colnum]))

for (i in 1:length(nutr_values)) {
  if (is.na(nutr_values[i]) || is.infinite(nutr_values[i])) { 
    nutr_values[i] = 0
  }
}
gap.plot(nutr_values, gap = gapVar, gap.axis = 'y', type = 'l', xlab = "Time", ylab = "Fraction of baseline", main = nutrientVar, ytics = gapVect,xaxt='n',xtics = c(0))#ylim = c(0, 0.15), type = 'l')
divider = length(example3[,1])/4
axis(1, at = c(example3[1,1], example3[divider,1], example3[divider*2, 1], example3[divider*3, 1], example3[divider*4, 1]), labels = c(as.character(as.Date(example3[1,1])), as.character(as.Date(example3[divider,1])), as.character(as.Date(example3[divider*2,1])), as.character(as.Date(example3[divider*3,1])), as.character(as.Date(example3[divider*4,1]))))

for (j in 1:length(sim_folders)) { # Iterate through sim folders
  for (k in 1:length(folders_to_find2[,1])) {
    if (sim_folders[j] == folders_to_find2[k,1]) {
      curr_sim2 <- read.csv(paste0(curr_directory, "/Sims/", sim_folders[j], "/Results/timeseries_nutrient.csv")) # Add/change as you desire
      nutr_values = (curr_sim2[,colnum] / (baseline_nutrient[,colnum]))
      
      for (i in 1:length(nutr_values)) {
        if (is.na(nutr_values[i]) || is.infinite(nutr_values[i])) { 
          nutr_values[i] = 0
        }
      }
      gap.plot(nutr_values, gap = gapVar, gap.axis = 'y', type = 'l', xlab = "Time", col = cols[counter3], ylab = "Fraction of baseline", main = nutrientVar, ytics = gapVect,xaxt='n',xtics = c(0), add = TRUE,lty= ltys[counter3])#ylim = c(0, 0.15), type = 'l')
      
      new_counter = new_counter + 1
      aggregate3 <- cbind(aggregate3, curr_sim2[,colnum])
      allconditions[counter3 + 1,] <- folders_to_find2[k,2:length(folders_to_find2[1,])]#eval(as.name(conditions[m]))[j,1]
      legendlabels[counter3] = allconditions[counter3 + 1, 1] # Needs to be changed for whatever the toggle var is
      counter3 = counter3 + 1
      break
    }
  }
}

legend("topright",c("setup",legendlabels), fill = c("black", cols), ncol=9, cex=0.75)
