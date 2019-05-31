# Grab the simulation results quickly! ####
# Written by Arianna Krinos, last edits by KJF on 23 May 2017

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
var1 <- "AirTemp_conditions" #listing as variable means you don't need eval(as.name()) or the deparse below
var2 <- "FLOW_conditions" #"Rain_conditions"
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
      if (z > 2 && folders_to_find2[counter, z] != 0) { #&& folders_to_find2[counter,z] != 1) {
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
example3 <- read.csv(paste0(curr_directory, "/Sims/", folders_to_find2[1,1], "/Results/timeseries_nutrient.csv"))
aggregate <- t(c(rep(0, length(example[1,]))))
aggregate2 <- t(c(rep(0, (length(example2[1,]) + length(folders_to_find2[1,]) - 1))))
aggregate3 <- c(rep(0, length(example3[,1]))) 
colnames(aggregate) <- colnames(example)
colnames(aggregate2) <- c((conditions), colnames(example2))
allconditions <- matrix(nrow = length(folders_to_find[,1]) + 1, ncol = length(conditions))

# PLOT HEAT MAPS AND SAVE TO OWN FOLDER ####
# COLLATE DATA FOR PERMUTATIONS OF VAR1 AND VAR2 (the ones you inputted)
counter2 <- 1

for (j in 1:length(sim_folders)) { # Iterate through sim folders
  for (k in 1:length(folders_to_find[,1])) {
    if (sim_folders[j] == folders_to_find[k,1]) {
      curr_sim0 <- read.csv(paste0(curr_directory, "/Sims/", sim_folders[j], "/Results/Sim__.csv")) # Add/change as you desire
      new_counter = new_counter + 1
      aggregate <- rbind(aggregate, curr_sim0)
      allconditions[counter2 + 1,] <- folders_to_find[k,2:length(folders_to_find[1,])]#eval(as.name(conditions[m]))[j,1]
      counter2 = counter2 + 1
      break
    }
  }
  # THIS IS YEARLY DATA FOR VAR1
  for (k in 1:length(folders_to_find2[,1])) {
    if (sim_folders[j] == folders_to_find2[k,1]) {
      curr_sim1 <- read.csv(paste0(curr_directory, "/Sims/", sim_folders[j], "/Results/Data_By_Time_GRAPLEready.csv"))
      curr_sim1 <- curr_sim1[1:(length(curr_sim1[,1]) - 1),]
      new_counter_2 <- new_counter_2 + 1
      
      blankcol <- rep(0, length(curr_sim1[,1]))
      blankcols <- rep(0, length(curr_sim1[,1]))
      for (p in 1:(length(folders_to_find2[1,])-2)) {
        blankcols <- cbind(blankcols, blankcol)
      }
      curr_sim1 <- cbind(blankcols, curr_sim1)
      for (i in 1:length(curr_sim1[,1])) {
        curr_sim1[i,1:(length(folders_to_find2[1,])-1)] <- folders_to_find2[k,2:length(folders_to_find2[1,])]
      }
      colnames(curr_sim1) <- c((conditions), colnames(example2))
      aggregate2 <- rbind(aggregate2, curr_sim1)
    }
  }
}
allconditions[1,] <- c(rep(0, length(allconditions[1,])))
colnames(allconditions) <- conditions
allconditions <- allconditions[1:length(aggregate[,1]),]
aggregate <- cbind(allconditions, aggregate)
aggregate <- aggregate[2:length(aggregate[,1]),]
aggregate2 <- aggregate2[2:length(aggregate2[,1]),]
names <- colnames(aggregate)

# Units and names to be added!!

newdir <- dir.create(paste0('Compare_14April17__', as.name(var1), as.name(var2))) # change the directory as needed
setwd(paste0(getwd(), '/Compare_14April17__', as.name(var1), as.name(var2)))

for (column in 1:length(aggregate[1,])) {
  units <- c(rep(0, length(conditions)), rep("deg C", 9), rep("mg/L", 22), rep("ug/L", 4), rep("ug/L", 26), 
             rep('WIP', 2), rep("mg/L", 6), rep("kg",4), rep("mg/L", 6), rep("kg", 4), rep("mg/L", 6),
             rep("kg", 4), rep("WIP", 12))
  all <- cbind(names, units)
  for (c in 1:length(colnames(allconditions))) {
    if (as.name(var1) == colnames(allconditions)[c]) {
      togglevar <- allconditions[2:length(allconditions[,1]),c]
      break
    }
  }
  for (d in 1:length(colnames(allconditions))) {
    if (as.name(var2) == colnames(allconditions)[d]) {
      comparevar <- allconditions[2:length(allconditions[,1]),d]
      break
    }
  }
  uniquevars_toggle <- sort.int(as.numeric(unique(togglevar)), decreasing = FALSE)
  uniquevars_compare <- sort.int(as.numeric(unique(comparevar)), decreasing = FALSE)
  totalcomparedata <- matrix(nrow = length(folders_to_find[,1]), ncol = 3)
  totalcomparedata <- data.frame(totalcomparedata)
  #column <- 38 # Column of var of interest in aggregate - if you're looking at aggregate 2 subtract one: 38, 42, 50, 17, 18, 19
  toggle_counter <- 1
  toggle_curr <- togglevar[toggle_counter]
  counter_new <- 1
  comparedata <- matrix(0, nrow = length(uniquevars_toggle), ncol = length(uniquevars_compare))
  row.names(comparedata) <- uniquevars_toggle
  colnames(comparedata) <- uniquevars_compare
  # Organize results for a particular set of factors into a matrix 
  for (toggle_counter in 1:length(uniquevars_toggle)) {
    toggle_curr <- uniquevars_toggle[toggle_counter]
    for (z in 1:length(togglevar)) {
      if (togglevar[z] == toggle_curr) {
        for (y in 1:length(uniquevars_compare)) {
          if (comparevar[z] == uniquevars_compare[y]) {
            curr2 = y
          }
          for (f in 1:length(aggregate[,1])) {
            if (comparevar[z] == aggregate[f, d] && toggle_curr == aggregate[f, c]) {
              find = f
            }
          }
        }
        comparedata[toggle_counter, curr2] <- aggregate[find, column]
        totalcomparedata[counter_new,] <- c(toggle_curr, comparevar[z], aggregate[find,column])
        counter_new = counter_new + 1
      }
    }
  }
  
  jet.colors <-
    colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                       "#7FFF7F", "yellow", "#FF7F00", "red"))(128)
  par(cex.lab = 1)
  par(cex.axis = 1)
  par(mar = c(3,3,3,3))
  png(file = paste0(as.character(all[column, 1]), '_', var1, '_', var2, '_8Apr17.png'))
  try (heatmap.2(comparedata, density.info = 'none', dendrogram = 'none', Rowv = FALSE, Colv = FALSE, 
                 col = jet.colors, key.title = "Gradient", key.xlab = as.character(all[column, 2]), 
                 main = as.character(all[column, 1]), ylab = paste0(var1, ' Increment'), xlab = paste0(var2, ' Increment'), 
                 tracecol = NA, cex.lab = 1, cex.axis = 1, margins = c(6,6), labRow = round(as.numeric(rownames(comparedata)),2), 
                 labCol = round(as.numeric(colnames(comparedata)),2)), silent = TRUE)
  #breaks = seq(1400,2000, length.out = 129)) #To set axis during standardization process
  dev.off()
}

# ------------------ Sort data by date data

newdir <- dir.create(paste0('Compare_Yearly_13April17_', as.name(var1)))
setwd(paste0(getwd(), '/Compare_Yearly_13April17_', as.name(var1)))
for (column2 in (length(conditions) + 2):length(aggregate2[1,])) {
  for (c in 1:length(colnames(allconditions))) {
    if (as.name(var1) == colnames(allconditions)[c]) {
      togglevar <- allconditions[length(allconditions[,1]),c]
      break
    }
  }
  d <- 3
  togglevar <-aggregate2[,c]
  comparevar <- aggregate2[,d]
  uniquevars_toggle <- sort.int(as.numeric(unique(togglevar)), decreasing = FALSE)
  uniquevars_compare <- sort.int(as.numeric(unique(aggregate2[,d])), decreasing = FALSE) # look at dates
  totalcomparedata2 <- matrix(nrow = length(folders_to_find[,1]), ncol = 3)
  totalcomparedata2 <- data.frame(totalcomparedata)
  #column <- 38 # Column of var of interest in aggregate - if you're looking at aggregate 2 subtract one: 38, 42, 50, 17, 18, 19
  toggle_counter <- 1
  toggle_curr <- togglevar[toggle_counter]
  counter_new <- 1
  comparedata2 <- matrix(0, nrow = length(uniquevars_toggle), ncol = length(uniquevars_compare))
  row.names(comparedata2) <- uniquevars_toggle
  colnames(comparedata2) <- uniquevars_compare
  # Organize results for a particular set of factors into a matrix 
  for (toggle_counter in 1:length(uniquevars_toggle)) {
    toggle_curr <- uniquevars_toggle[toggle_counter]
    for (z in 1:length(togglevar)) {
      if (togglevar[z] == toggle_curr) {
        for (y in 1:length(uniquevars_compare)) {
          if (comparevar[z] == uniquevars_compare[y]) {
            curr2 = y
          }
          for (f in 1:length(aggregate2[,1])) {
            if (comparevar[z] == aggregate2[f, d] && toggle_curr == aggregate2[f, c]) {
              find = f
            }
          }
        }
        comparedata2[toggle_counter, curr2] <- as.numeric(aggregate2[find, column2])
        totalcomparedata2[counter_new,] <- c(toggle_curr, comparevar[z], as.numeric(aggregate2[find,column2]))
        counter_new = counter_new + 1
      }
    }
  }
  #comparedata2 <- comparedata2[,2:length(comparedata2[1,])]
  title <- colnames(aggregate2)[column2] #comment this out if you don't have aggregate2 already stored!!
  par(cex.lab = 1)
  par(cex.axis = 1)
  par(mar = c(3,3,3,3))
  png(file = paste0(title, '_', var1, '_year', '_14Apr17.png'))
  try(heatmap.2((comparedata2), density.info = 'none', dendrogram = 'none', Rowv = FALSE, Colv = FALSE, 
                col = jet.colors, key.title = "Gradient", key.xlab = 'units', 
                main = title, ylab = paste0(var1, ' Increment'), xlab = paste0('Year'), 
                tracecol = NA, cex.lab = 1, cex.axis = 1, margins = c(6,6), labRow = round(as.numeric(rownames(comparedata2)),2), 
                labCol = round(as.numeric(colnames(comparedata2)),2)), silent = TRUE)
  dev.off()
}