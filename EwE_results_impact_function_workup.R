## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~     Initialisation     ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

rm(list=ls())

## Libraries
library(tidyverse)
library(reshape2)
library(devtools)
# Plotting
library(ggplot2)
library(RColorBrewer)
library(viridis)
library(hrbrthemes)
# PCA and Clustering
library(factoextra)
library (FactoMineR)
library(corrplot)
library(ape)
# Loop for plotting the EwE fitted data
library(data.table)
library(dbplyr)
library(ggpubr)
library(gridExtra)

inDir <- "/Users/ful083/Work/Cumulative_Impacts_&_IEA_&_ERA/FRDC_cum_impacts_fisheries_project/Analysis_&_Workup/EwE_sims/Results/Multisim"
outDir <- "/Users/ful083/Work/Cumulative_Impacts_&_IEA_&_ERA/FRDC_cum_impacts_fisheries_project/Analysis_&_Workup/EwE_sims/"

setwd(outDir)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ~       Read data        ~ ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
fleetcodes <- c("Tw", "NTw", "L", "NSW", "V", "Sc", "Sq", "Tp", "DS", "TL", "Rec")
nfleet <- length(fleetcodes)
nYr <- 50
nMth <- 600
GrpNames <- c("Year","Toothed_whale","Baleen_whale","Seal","Seabirds","Penguins","Tuna_billfish","Pelagic_sharks","Demersal_sharks","Rays","Warehous","Redbait","Redfish","Ling","Dories","Jack_mackerel","Jackass_morwong","Flathead","Gemfish","ShOceanPerch","Chinaman_leatherjacket","Cucumberfish","Whiting","Cardinal","ShSmInvertFeeder","ShSmPredator","ShMedInvertFeeder","ShMedPredator","ShLInvertFeeder","ShLPredator","Blue_eye_trevalla","Blue_grenadier","SlopeOceanPerch","Deepsea_Cod","Oreos","SlopeSmInvertFeeder","SlopeSmPredator","SlopeMInverFeeder","SlopeMPredator","SlopeLInvertFeeder","SlopeLPredator","PelSmInvertFeeder","PelMInvertFeeder","PelMPredator","PelLInvertFeeder","PelLPredator","Mesopelagics","Squid","Commercial_Prawns","Macrobenthos","Megabenthos","Polychaeta","Gelatinous_nekton","Euphausids","L_zooplankton","Sm_zooplankton","Primary_producers","Benthic_producer","Detritus","Discards")

# Read the full list of directories
dirs <- list.dirs(path = inDir, full.names = TRUE, recursive = FALSE)
ndirs <- length(dirs)

# Helpful function for loading EwE files
# Skip line function. this automatically get the number of rows based on the ewe version
# param file EwE .csv output file
# return Number of lines to skip
skip_lines <- function(file,flag){
  strs  <- readLines(file)
  if(flag < 1) {
    lines <- grep('year\\\\group,', strs) -1
  } else {
    lines <- grep('timestep\\\\group,', strs) -1
  }
  return(lines)
}

# Create data store
#create data frame with 0 rows and 3 columns
FleetActive <- data.frame(matrix(ncol = nfleet, nrow = 0))
colnames(FleetActive) <- fleetcodes
noF <- 0

# Pick up first directory as a starting point
i <- 1

this_dir <- dirs[i]
# Snip out inDir name
this_dir <- gsub(inDir, '', this_dir)
this_dir <- gsub("/", '', this_dir)

# Loop over fleets to see what matches exist and what level fo effort being applied
thisEffortLevel <- 0  #If remains zero then must be noF case
for(eL in 2:36) {
  if(grepl(eL, this_dir, fixed=TRUE) == TRUE) {
    thisEffortLevel <- eL
  }
}

nFleetActive <- 0  # Number of fleets involved
for (nf in 1:nfleet) {
  check_name <- fleetcodes[nf]
  #cat("Check",check_name,"vs",this_dir,"\n")
  if(grepl(check_name, this_dir, fixed=TRUE) == TRUE) {
    nFleetActive <- nFleetActive + 1
    FleetActive[1,nf] <- 1
  }
}

# Counts of the fleets included - set case as directs the code later on
FleetActive[is.na(FleetActive)] <- 0
Case <- rowSums(FleetActive)

# Read in the relevant files
CatchFile <- paste(dirs[i],"/","catch_annual.csv",sep="")
skip_this <- skip_lines(CatchFile,0)
dfcatch <- read.csv(CatchFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
names(dfcatch)[1:60] <- GrpNames   # hanges the name of the first column to year instead of year//group and all columns to names not numbers

BioFile <- paste(dirs[i],"/","biomass_annual.csv",sep="")
skip_this <- skip_lines(BioFile,0)
dfbio <- read.csv(BioFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
names(dfbio)[1:60] <- GrpNames   # changes the name of the first column to year instead of year//group and all columns to names not numbers

# Assume skip_this matches for the BioFile
BioDivFile <- paste(dirs[i],"/","kemptonsq_annual.csv",sep="")
dfdiv <- read.csv(BioDivFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
names(dfdiv)[2] <- "BioDivQ"   

## Combined data
dfComboBio <- dfbio[50,]
dfComboBio <- subset(dfComboBio, select = -c(1)) #remove Year 
dfQ <- dfdiv$BioDivQ[[50]]
dfComboBio$Biodiv <- dfQ
dfComboBio$NFleet <- Case #add in number of fisheries active
dfComboBio$EffortLevel <- thisEffortLevel #add in effort level 
dfComboBio <- merge.data.frame(dfComboBio, FleetActive)
row.names(dfComboBio)[1] <- 1

dfComboCatch <- dfcatch[50,]
dfComboCatch <- subset(dfComboCatch, select = -c(1)) #remove Year 
dfComboCatch$NFleet <- Case #add in number of fisheries active
dfComboCatch$EffortLevel <- thisEffortLevel #add in effort level 
dfComboCatch <- merge.data.frame(dfComboCatch, FleetActive)
row.names(dfComboCatch)[1] <- 1

# Skip monthly info for now
#CatchFile <- paste(dirs[i],"/","catch_monthly.csv",sep="")
#skip_this <- skip_lines(CatchFile,1)
#dfcatchMonth <- read.csv(CatchFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
#names(dfcatchMonth)[1:60] <- GrpNames   # hanges the name of the first column to year instead of year//group and all columns to names not numbers
#
#BioFile <- paste(dirs[i],"/","biomass_monthly.csv",sep="")
#skip_this <- skip_lines(BioFile,1)
#dfbioMonth <- read.csv(BioFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
#names(dfbioMonth)[1:60] <- GrpNames   # changes the name of the first column to year instead of year//group and all columns to names not numbers
#
# Assume skip_this matches for the BioFile
#BioDivFile <- paste(dirs[i],"/","kemptonsq_monthly.csv",sep="")
#dfdivMonth <- read.csv(BioDivFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
#names(dfdivMonth)[2] <- "BioDivQ"   



# Run teh rest of the directories
for (i in 2:ndirs) {
  this_dir <- dirs[i]
  # Snip out inDir name
  this_dir <- gsub(inDir, '', this_dir)
  this_dir <- gsub("/", '', this_dir)

  # Loop over fleets to see what matches exist and what level fo effort being applied
  thisEffortLevel <- 0  #If all remain zero then must be noF case
  for(eL in 2:36) {
    if(grepl(eL, this_dir, fixed=TRUE) == TRUE) {
      thisEffortLevel <- eL
    }
  }
  
  nFleetActive <- 0 # Reinit
  FleetActive[,] <- 0
  for (nf in 1:nfleet) {
    check_name <- fleetcodes[nf]
    #cat("Check",check_name,"vs",this_dir,"\n")
    if(grepl(check_name, this_dir, fixed=TRUE) == TRUE) {
      nFleetActive <- nFleetActive + 1
      FleetActive[1,nf] <- 1
    }
  }
  
  # Counts of the fleets included - set case as directs the code later on
  FleetActive[is.na(FleetActive)] <- 0
  Case <- rowSums(FleetActive)
  
  # Read in the relevant files
  CatchFile <- paste(dirs[i],"/","catch_annual.csv",sep="")
  skip_this <- skip_lines(CatchFile,0)
  dfcatch <- read.csv(CatchFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  names(dfcatch)[1:60] <- GrpNames   # hanges the name of the first column to year instead of year//group and all columns to names not numbers
  
  BioFile <- paste(dirs[i],"/","biomass_annual.csv",sep="")
  skip_this <- skip_lines(BioFile,0)
  dfbio <- read.csv(BioFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  names(dfbio)[1:60] <- GrpNames   # changes the name of the first column to year instead of year//group and all columns to names not numbers
  
  # Assume skip_this matches for the BioFile
  BioDivFile <- paste(dirs[i],"/","kemptonsq_annual.csv",sep="")
  dfdiv <- read.csv(BioDivFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  names(dfdiv)[2] <- "BioDivQ"   
  
  # Skip monthly info for now
  #CatchFile <- paste(dirs[i],"/","catch_monthly.csv",sep="")
  #skip_this <- skip_lines(CatchFile,1)
  #dfcatchMonth <- read.csv(CatchFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  #names(dfcatchMonth)[1:60] <- GrpNames   # hanges the name of the first column to year instead of year//group and all columns to names not numbers
  #  
  #BioFile <- paste(dirs[i],"/","biomass_monthly.csv",sep="")
  #skip_this <- skip_lines(BioFile,1)
  #dfbioMonth <- read.csv(BioFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  #names(dfbioMonth)[1:60] <- GrpNames   # changes the name of the first column to year instead of year//group and all columns to names not numbers
  #
  # Assume skip_this matches for the BioFile
  #BioDivFile <- paste(dirs[i],"/","kemptonsq_monthly.csv",sep="")
  #dfdivMonth <- read.csv(BioDivFile, header = T, skip = skip_this, check.names = FALSE) # because csv file has headers - will need to skip the first 9 rows; check names makes sure an X isn't added before the number for the species
  #names(dfdivMonth)[2] <- "BioDivQ"   
  
  ## Store the data
  # Original idea was to store the entire nested list, but do we really need the time series? 
  # As equilibrium model can just store the final result

  # Original idea
  # my_nested_list[[i]] <- list(EffortLevel, dfcatch, dfbio, dfdiv, dfcatchMonth, dfbioMonth, dfdivMonth)
  # To get information back out of this
  # my_nested_list[[i]][[1]] returns the EffortLevel vector
  # my_nested_list[[i]][[2]] returns the dataframe dfcatch
  # my_nested_list[[i]][[3]] returns the dataframe dfbio
  # my_nested_list[[i]][[4]] returns the dataframe dfdiv
  # my_nested_list[[i]][[5]] returns the dataframe dfcatchMonth
  # my_nested_list[[i]][[6]] returns the dataframe dfbioMonth
  # my_nested_list[[i]][[7]] returns the dataframe dfdivMonth
  
  # New idea - store final line of data, and start the list with the Case type 
  # i.e. number of fisheries involved and the effort level (given it is identical across fisheries in this case)
  
  ## Combined data
  dftmp <- dfbio[50,]
  dftmp <- subset(dftmp, select = -c(1)) #remove Year 
  dfQ <- dfdiv$BioDivQ[[50]]
  dftmp$Biodiv <- dfQ
  dftmp$NFleet <- Case #add in number of fisheries active
  dftmp$EffortLevel <- thisEffortLevel #add in effort level 
  dftmp <- merge.data.frame(dftmp, FleetActive)
  row.names(dftmp)[1] <- i
  
  dforig <- dfComboBio
  dfComboBio <- rbind(dforig, dftmp)
  
  dftmp <- dfcatch[50,]
  dftmp <- subset(dftmp, select = -c(1)) #remove Year 
  dftmp$NFleet <- Case #add in number of fisheries active
  dftmp$EffortLevel <- thisEffortLevel #add in effort level 
  dftmp <- merge.data.frame(dftmp, FleetActive)
  row.names(dftmp)[1] <- i
  
  dforig <- dfComboCatch
  dfComboCatch <- rbind(dforig, dftmp)

  # Message sp can track load process
  cat("Loading",i,"\n")
}

# Write out files
write.csv(dfComboBio, file = "Compiled_Biomass_Responses_EwE_sims.csv", row.names = FALSE)
write.csv(dfComboCatch, file = "Compiled_Catch_Responses_EwE_sims.csv", row.names = FALSE)

# Exploratory plots to look for patterns in  impact response functions
# Due to the number of groups, will need to break into "pages"
# Also filter by fleet and order by number of fleets involved
SpeciesNames <- c("Toothed_whale","Baleen_whale","Seal","Seabirds","Penguins","Tuna_billfish","Pelagic_sharks","Demersal_sharks","Rays","Warehous","Redbait","Redfish","Ling","Dories","Jack_mackerel","Jackass_morwong","Flathead","Gemfish","ShOceanPerch","Chinaman_leatherjacket","Cucumberfish","Whiting","Cardinal","ShSmInvertFeeder","ShSmPredator","ShMedInvertFeeder","ShMedPredator","ShLInvertFeeder","ShLPredator","Blue_eye_trevalla","Blue_grenadier","SlopeOceanPerch","Deepsea_Cod","Oreos","SlopeSmInvertFeeder","SlopeSmPredator","SlopeMInverFeeder","SlopeMPredator","SlopeLInvertFeeder","SlopeLPredator","PelSmInvertFeeder","PelMInvertFeeder","PelMPredator","PelLInvertFeeder","PelLPredator","Mesopelagics","Squid","Commercial_Prawns","Macrobenthos","Megabenthos","Polychaeta","Gelatinous_nekton","Euphausids","L_zooplankton","Sm_zooplankton","Primary_producers","Benthic_producer","Detritus","Discards","Biodiv")
nSP <- length(SpeciesNames)
starti <- nSP + 1
this_ncol <- length(colnames(dfComboBio))
cnum <- seq(nSP+1, this_ncol, by=1)
this_cname <- as.character(cnum)

this_ncolcat <- length(colnames(dfComboCatch))
starticat <- nSP # As no biodiv to be accounting for

nPanel <- 12  # one for each fleet and overall
nLoop <- nSP # Each group plus bulk biodiv'
maxN <- max(dfComboBio$NFleet)

for (iLoop in 1:nLoop) {
  
  check_name <- SpeciesNames[iLoop]
  cat("Doing",check_name,"Bio","\n")
  
  ## Biomass
  outPlotName <- paste("Bio-per-Nfleet-",check_name,".png",sep="")
  dfextract <- dfComboBio[,c(iLoop,starti:this_ncol)]
  colnames(dfextract)[1] <- c("Value")
  
  # Overall 
  ggplot(data = dfextract, mapping = aes(x = EffortLevel, y = Value)) +
    geom_point(aes(color = factor(NFleet))) + 
    geom_smooth(aes(color = factor(NFleet), fill = factor(NFleet)), method = "loess") + 
    scale_color_viridis(discrete = TRUE, option = "D") +
    scale_fill_viridis(discrete = TRUE) +
    facet_wrap (factor(NFleet)~., scales="free_y") +
    #expand_limits(y=0) +
    labs( x = 'Pressure level', y = 'Biomass') + theme(
      plot.title = element_text(face="bold"),
      axis.title.x = element_text(face="bold"),
      axis.title.y = element_text(face="bold"),
      #legend.title = element_text(face="bold"),
      legend.position="none"
    )
  ggsave(file=outPlotName)
  
  #Now per Fleet
  for (nf in 1:maxN){
    nd <- 3 + nf  # as first three columns are Value, NFleet, EffortLevel
    dfsub <- dplyr::filter(dfextract, dfextract[,nd]>0) # Suck out cases where this fishery is non-zero in teh effort mix
    outPlotName <- paste("Bio-",check_name,"-",fleetcodes[nf],".png",sep="")
    ggplot(data = dfsub, mapping = aes(x = EffortLevel, y = Value)) +
      geom_point(aes(color = factor(NFleet))) + 
      geom_smooth(aes(color = factor(NFleet), fill = factor(NFleet)), method = "loess") + 
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_fill_viridis(discrete = TRUE) +
      facet_wrap (factor(NFleet)~., scales="free_y") +
      #expand_limits(y=0) +
      labs( x = 'Pressure level', y = 'Biomass') + theme(
        plot.title = element_text(face="bold"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        #legend.title = element_text(face="bold"),
        legend.position="none"
      )
    ggsave(file=outPlotName)
    
  }
  
  ## Catch
  if(iLoop < nLoop) {
    outPlotName <- paste("Catch-per-Nfleet-",check_name,".png",sep="")
    dfextract <- dfComboCatch[,c(iLoop,starticat:this_ncolcat)]
    colnames(dfextract)[1] <- c("Value")
    maxC <- max(dfextract$Value)
  
    if(maxC > 0) {
      cat("and catch","\n")
      # Overall 
      ggplot(data = dfextract, mapping = aes(x = EffortLevel, y = Value)) +
        geom_point(aes(color = factor(NFleet))) + 
        geom_smooth(aes(color = factor(NFleet), fill = factor(NFleet)), method = "loess") + 
        scale_color_viridis(discrete = TRUE, option = "D") +
        scale_fill_viridis(discrete = TRUE) +
        facet_wrap (factor(NFleet)~., scales="free_y") +
        labs( x = 'Pressure level', y = 'Catch') + theme(
          plot.title = element_text(face="bold"),
          axis.title.x = element_text(face="bold"),
          axis.title.y = element_text(face="bold"),
          legend.position="none"
        )
      ggsave(file=outPlotName)
  
      #Now per Fleet
      for (nf in 1:maxN){
        nd <- 3 + nf  # as first three columns are Value, NFleet, EffortLevel
        dfsub <- dplyr::filter(dfextract, dfextract[,nd]>0) # Suck out cases where this fishery is non-zero in teh effort mix
        outPlotName <- paste("Catch-",check_name,"-",fleetcodes[nf],".png",sep="")
        ggplot(data = dfsub, mapping = aes(x = EffortLevel, y = Value)) +
          geom_point(aes(color = factor(NFleet))) + 
          geom_smooth(aes(color = factor(NFleet), fill = factor(NFleet)), method = "loess") + 
          scale_color_viridis(discrete = TRUE, option = "D") +
          scale_fill_viridis(discrete = TRUE) +
          facet_wrap (factor(NFleet)~., scales="free_y") +
          labs( x = 'Pressure level', y = 'Catch') + theme(
            plot.title = element_text(face="bold"),
            axis.title.x = element_text(face="bold"),
            axis.title.y = element_text(face="bold"),
            legend.position="none"
            )
        ggsave(file=outPlotName)
      }
    }
  }
}

# Now want a transition matrix of function to explain how values of catch, 
# biomass, biodiversity change as effort per fishery and the number of fisheries increases?
# Assume contents of the transition matrix need to be of the form
# transition_Eff1_to_Eff2 = Value_Eff2 / Value_Eff1

# There will be some smart vectorised way, but I will just loop over cases
